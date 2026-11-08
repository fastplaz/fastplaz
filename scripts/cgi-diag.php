<?php
/**
 * FastPlaz CGI binary checker
 *
 * Mendiagnosa kenapa binary CGI (*.bin) gagal jalan di shared hosting
 * (misal cPanel tanpa SSH) dan hanya menampilkan "500 Internal Server Error".
 * Menampilkan versi glibc & OS server, lalu menjalankan binary dengan
 * environment CGI minimal dan memperlihatkan stdout/stderr aslinya,
 * misal: "version `GLIBC_2.34' not found".
 *
 * Pemakaian:
 *   1. Ganti KEY di bawah dengan string acak.
 *   2. Upload ke folder yang sama dengan binary (misal public_html/).
 *   3. Buka: https://domain/cgi-diag.php?k=KEY&bin=nama.bin
 *      (bin default: test.bin)
 *   4. HAPUS file ini setelah selesai, karena menjalankan perintah shell.
 */

const KEY = '';   // WAJIB diisi; jika kosong script menolak jalan

if (KEY === '' || !hash_equals(KEY, (string)($_GET['k'] ?? ''))) {
  http_response_code(404);
  exit;
}
header('Content-Type: text/plain; charset=utf-8');

// hanya nama file di folder ini, tanpa path
$bin = basename((string)($_GET['bin'] ?? 'test.bin'));
if (!preg_match('/^[A-Za-z0-9._-]+$/', $bin)) {
  exit("nama binary tidak valid\n");
}

function run($cmd, $env = null) {
  echo "\$ $cmd\n";
  if (!function_exists('proc_open')) { echo "proc_open disabled\n\n"; return; }
  $p = proc_open($cmd, [1 => ['pipe', 'w'], 2 => ['pipe', 'w']], $pipes, __DIR__, $env);
  if (!is_resource($p)) { echo "gagal menjalankan\n\n"; return; }
  $out = stream_get_contents($pipes[1]);
  $err = stream_get_contents($pipes[2]);
  $code = proc_close($p);
  echo "exit: $code\n--- stdout ---\n" . substr($out, 0, 3000) . "\n--- stderr ---\n$err\n\n";
}

$host = $_SERVER['SERVER_NAME'] ?? 'localhost';
$cgiEnv = [
  'GATEWAY_INTERFACE' => 'CGI/1.1',
  'REQUEST_METHOD'    => 'GET',
  'QUERY_STRING'      => '',
  'SERVER_NAME'       => $host,
  'HTTP_HOST'         => $host,
  'SCRIPT_NAME'       => '/' . $bin,
  'PATH_INFO'         => '',
  'PATH'              => '/usr/bin:/bin',
];
$qbin = escapeshellarg('./' . $bin);

echo "uname: " . php_uname() . "\n";
echo "php: " . PHP_VERSION . "\n";
echo "disable_functions: " . ini_get('disable_functions') . "\n\n";

run('ldd --version 2>&1 | head -1');
run('cat /etc/os-release /etc/redhat-release 2>/dev/null | head -3');
run('ls -la ' . $qbin);
run('file ' . $qbin . ' 2>&1');
// versi glibc yang dibutuhkan binary (jika objdump tersedia)
run('objdump -T ' . $qbin . " 2>&1 | grep -o 'GLIBC_[0-9.]*' | sort -uV | tail -3");
run('ldd ' . $qbin . ' 2>&1');
run($qbin, $cgiEnv);
