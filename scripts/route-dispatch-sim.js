#!/usr/bin/env node
/*
  route-dispatch-sim.js

  Compares how a FastPlaz app picks the module for a request before and
  after the dispatcher change "check route patterns before a module name
  from the path" (fastplaz_handler, MatchRoute()).

  Old: a PathInfo equal to a registered module name (or an empty PathInfo,
       which becomes the default module "main") went to that module
       directly; route patterns were only checked otherwise.
  New: route patterns are checked first; the lookup by name only applies
       when no pattern matches.
  Requests with ?mod=<module> behave the same in both and are not simulated.

  The routes are read from the initialization section of each unit:
    Route['pattern'] := TClass;
    Route.Add('pattern', TClass, ...);
    Route.Add('name', 'pattern', TClass, ...);
  plus the routes registered by units under <root>/controllers that the
  file uses.

  Usage:
    node scripts/route-dispatch-sim.js <root> [routes unit ...]
    node scripts/route-dispatch-sim.js <root> --paths=/,/command,/abc [routes unit ...]

    <root>         project source folder; without units the script reads
                   <root>/routes/*_routes.pas
    routes unit    path of a unit to check (relative to <root> or absolute)
    --paths=a,b    extra PathInfo values to check besides '' and /<module>

  Exit code: 0 when every unit dispatches the same, 1 when a path changed,
  2 on a usage error.
*/
'use strict';

const fs = require('fs');
const path = require('path');

// --- arguments --------------------------------------------------------------

const args = process.argv.slice(2);
let extraPaths = ['/', '/main'];
const positional = [];
for (const a of args) {
  if (a.startsWith('--paths=')) {
    extraPaths = a.slice(8).split(',').filter(Boolean);
  } else if (a === '-h' || a === '--help') {
    printUsage();
    process.exit(0);
  } else {
    positional.push(a);
  }
}
if (positional.length < 1) {
  printUsage();
  process.exit(2);
}
const root = path.resolve(positional[0]);

function printUsage() {
  console.log('usage: node scripts/route-dispatch-sim.js <root> [--paths=/,/main] [routes unit ...]');
}

// --- FastPlaz name rules ----------------------------------------------------

// common.pas CleanUrl()
function cleanUrl(url) {
  let r = url.trim().toLowerCase();
  const chars = [' ', ',', '?', '!', '.', "'", '+', '^', '"', '\r', '\n', '/', '\\',
    '(', ')', '[', ']', '*', '$'];
  for (const c of chars) r = r.split(c).join('-');
  for (const c of ['---', '--']) r = r.split(c).join('-');
  return r;
}

// TRoute.Add(ModuleName, ModuleClass): the module name made from a pattern
function nameFromPattern(pattern) {
  return cleanUrl(pattern).split('_').join('').replace(/[-|]/g, '_');
}

// --- reading routes ---------------------------------------------------------

function stripComments(src) {
  return src
    .replace(/\{(?!\$)[^}]*\}/g, '')     // { comment }, keeps {$directive}
    .replace(/\(\*[\s\S]*?\*\)/g, '')    // (* comment *)
    .replace(/\/\/[^\n]*/g, '');         // // comment
}

function registrations(file, seen = new Set()) {
  if (seen.has(file) || !fs.existsSync(file)) return [];
  seen.add(file);
  const src = stripComments(fs.readFileSync(file, 'utf8'));
  const regs = [];

  // used units register their routes first (unit initialization order)
  for (const m of src.matchAll(/\buses\b([\s\S]*?);/gi)) {
    for (const unit of m[1].match(/[A-Za-z_][A-Za-z0-9_]*/g) || []) {
      const candidate = path.join(root, 'controllers', unit.toLowerCase() + '.pas');
      if (fs.existsSync(candidate)) regs.push(...registrations(candidate, seen));
    }
  }

  const parts = src.split(/\binitialization\b/i);
  if (parts.length < 2) return regs;
  const body = parts[1];
  const re = /Route\s*\[\s*'([^']*)'\s*\]\s*:=|Route\.Add\s*\(\s*'([^']*)'\s*,\s*(?:'([^']*)'|[A-Za-z_][A-Za-z0-9_]*)/g;
  for (const m of body.matchAll(re)) {
    if (m[1] !== undefined) regs.push({ name: nameFromPattern(m[1]), pattern: m[1] });
    else if (m[3] !== undefined) regs.push({ name: m[2], pattern: m[3] });
    else regs.push({ name: nameFromPattern(m[2]), pattern: m[2] });
  }
  return regs;
}

// --- dispatch ---------------------------------------------------------------

// TRegExpr syntax to JavaScript: (?P<name>...) -> (?<name>...)
function toJsRegExp(pattern) {
  return new RegExp(pattern.replace(/\(\?P</g, '(?<'));
}

function matchRoute(regs, modules, pathInfo) {
  for (const r of regs) {
    let re;
    try {
      re = toJsRegExp(r.pattern);
    } catch (e) {
      return `REGEX-ERROR ${r.pattern}: ${e.message}`;
    }
    if (re.test(pathInfo) && modules.has(r.name)) return r.name;
  }
  return '';
}

function dispatch(regs, pathInfo, isNew) {
  const modules = new Set(regs.map(r => r.name));
  // GetActiveModuleName: PathInfo without the leading slash, default "main"
  const active = pathInfo.replace(/^\/+/, '') || 'main';
  const urlPath = pathInfo.startsWith('/') ? pathInfo : '/' + pathInfo;
  const fallback = { module: '(fpweb by name)', how: 'fallback' };

  if (!isNew) {
    if (modules.has(active)) return { module: active, how: 'name' };
    const m = matchRoute(regs, modules, urlPath);
    return m ? { module: m, how: 'regex' } : fallback;
  }
  const m = matchRoute(regs, modules, urlPath);
  if (m) return { module: m, how: 'regex' };
  if (modules.has(active)) return { module: active, how: 'name' };
  return fallback;
}

// --- main -------------------------------------------------------------------

let files = positional.slice(1).map(f => path.resolve(root, f));
if (files.length === 0) {
  const dir = path.join(root, 'routes');
  if (!fs.existsSync(dir)) {
    console.error(`no routes folder in ${root}; pass the routes units`);
    process.exit(2);
  }
  files = fs.readdirSync(dir)
    .filter(f => /_routes\.pas$/i.test(f))
    .sort()
    .map(f => path.join(dir, f));
}

let total = 0;
for (const file of files) {
  const regs = registrations(file);
  const names = [...new Set(regs.map(r => r.name))].sort();
  const probes = [...new Set(['', ...names.map(n => '/' + n), ...extraPaths])];
  const diffs = [];
  for (const p of probes) {
    const o = dispatch(regs, p, false);
    const n = dispatch(regs, p, true);
    if (o.module !== n.module) {
      diffs.push(`    PathInfo ${JSON.stringify(p).padEnd(30)} old=${(o.module + ' [' + o.how + ']').padEnd(30)} new=${n.module} [${n.how}]`);
    }
  }
  const label = path.relative(root, file) || file;
  console.log(`${label.padEnd(30)} ${String(regs.length).padStart(2)} routes, ${diffs.length ? 'CHANGED' : 'same'}`);
  for (const d of diffs) console.log(d);
  total += diffs.length;
}
console.log(`paths with a different module: ${total}`);
process.exit(total ? 1 : 0);
