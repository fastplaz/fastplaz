Lazarus Menu Tool: AI Agent Plane by FastPlaz

AI Agent Plane untuk berkomunikasi dengan ai agent (spt: Claude Code, OpenCode, Pi, GitHub Copilot CLI).

nama fitur: AI Agent Plane
nama menu saat ini: FastPlaz > Agent Plane
lokasi menu: sebelum Database Explorer

source di: tools/src/ai_plane
branch: feature/aiplane-mcp

Dokumen terkait:
- [MCP.md](MCP.md): MCP server di IDE dan daftar tool yang sudah ada
- [AI_PLANE_TOOL_RUNDOWN.md](AI_PLANE_TOOL_RUNDOWN.md): desain dan roadmap IDE tools

## Konfigurasi:

terdapat form konfigurasi (FastPlaz > Configuration, tab "AI Harness") yang mengatur:
- default adapter harness/runtime yang digunakan: Claude Code/OpenCode/Pi/GitHub Copilot CLI
  adapter akan mungkin bertambah di masa depan
- default model yang digunakan (auto detect daftar model ketika memilih harness)
- default thinking effort (auto/off/minimal/low/medium/high/xhigh)
- IDE tools (MCP): aktif/tidak (`aiplane/mcp_enabled`) dan port (`aiplane/mcp_port`, default 17826)

terdapat tombol untuk "Run Test"

konfigurasi dijadikan satu dengan form "Fast Code Configuration", menggunakan TPageControl/TTabControl

## Use Case

ketika user pilih menu "FastPlaz > Agent Plane", akan muncul window chat (dockable).
ui/ux-nya seperti pada chat Anti Grafity.
terdapat:
- window conversation scrollable
- input chat
- info runtime adapter yang digunakan
- pilihan model yang bisa digunakan.

user bisa chat yang akan berinteraksi dengan runtime yang aktif saat itu.

## Tools

IDE menjalankan MCP server sendiri (http://127.0.0.1:17826/mcp) yang otomatis didaftarkan ke agent yang dijalankan dari AI Agent Plane. Lewat server ini agent bisa berinteraksi dengan Lazarus IDE.

Tool yang sudah ada (13):
- editor dan project: `open_file`, `get_active_editor`, `list_open_files`, `read_editor`, `project_info`, `list_project_units`
- build: `compile_project`, `build_project`, `clean_and_compile_project`, `get_messages`
- generator FastPlaz: `create_simple_controller`, `create_api_controller`, `create_model`

Detail tiap tool, argumen dan permission: [MCP.md](MCP.md).
Tool berikutnya (CodeTools, Database Explorer, debugger, dst): [AI_PLANE_TOOL_RUNDOWN.md](AI_PLANE_TOOL_RUNDOWN.md).

## App Tester

app tester yang perilakunya sama dengan window chat, agar tidak perlu recompile lazarus yang seringkali membutuhkan waktu lama:
- `tools/src/test/aiplane_tester/aiplane_tester.lpi`: LCL tester, window chat yang sama di luar IDE
- `tools/src/test/aiplane_cli.lpr`: tester di command line
- `tools/src/test/mock_acp_agent.lpr`: agent tiruan untuk test adapter ACP

## Komunikasi dengan adapter/runtime/harness

komunikasi dalam mode stream, sehingga user bisa langsung melihat apa yang terjadi di window chat.
