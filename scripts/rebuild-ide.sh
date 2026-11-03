#!/usr/bin/env bash
# Rebuild Lazarus IDE (Linux/macOS): build, strip debug info, start.
# LAZDIR bisa di-override: LAZDIR=/opt/lazarus ./rebuild-ide.sh

set -u

OS="$(uname -s)"

if [ -z "${LAZDIR:-}" ]; then
  for d in "$HOME/lazarus" /usr/share/lazarus /usr/lib/lazarus /opt/lazarus \
           /Applications/Lazarus /Developer/lazarus /usr/local/share/lazarus; do
    if [ -f "$d/lazarus.lpi" ] || [ -f "$d/ide/lazarus.lpi" ]; then
      LAZDIR="$d"
      break
    fi
  done
fi
if [ -z "${LAZDIR:-}" ] || [ ! -d "$LAZDIR" ]; then
  echo "Folder Lazarus tidak ditemukan, set LAZDIR dulu."
  exit 1
fi

LAZBUILD="$LAZDIR/lazbuild"
[ -x "$LAZBUILD" ] || LAZBUILD="$(command -v lazbuild || true)"
if [ -z "$LAZBUILD" ]; then
  echo "lazbuild tidak ditemukan."
  exit 1
fi

if pgrep -x lazarus >/dev/null 2>&1; then
  echo "Lazarus masih berjalan, tutup dulu IDE-nya."
  exit 1
fi

echo "=== Build Lazarus IDE ($LAZDIR) ==="
if ! "$LAZBUILD" --lazarusdir="$LAZDIR" --build-ide=; then
  echo
  echo "Build GAGAL."
  exit 1
fi

# lazbuild writes to the Lazarus folder when it is writable, otherwise to
# the user's config folder: take the newest one.
EXE=""
for f in "$LAZDIR/lazarus" "$HOME/.lazarus/bin/lazarus"; do
  if [ -f "$f" ] && { [ -z "$EXE" ] || [ "$f" -nt "$EXE" ]; }; then
    EXE="$f"
  fi
done
if [ -z "$EXE" ]; then
  echo "Binary lazarus tidak ditemukan setelah build."
  exit 1
fi

echo
echo "=== Strip debug info ==="
if command -v strip >/dev/null 2>&1; then
  if [ "$OS" = "Darwin" ]; then
    strip -S "$EXE" || echo "strip gagal, dilewati."
  else
    strip "$EXE" || echo "strip gagal, dilewati."
  fi
else
  echo "strip tidak ditemukan, dilewati."
fi

echo
echo "=== Start Lazarus ==="
cd "$LAZDIR" || exit 1
if [ "$OS" = "Darwin" ] && [ "$EXE" = "$LAZDIR/lazarus" ] && [ -d "$LAZDIR/lazarus.app" ]; then
  open "$LAZDIR/lazarus.app"
else
  nohup "$EXE" >/dev/null 2>&1 &
fi
