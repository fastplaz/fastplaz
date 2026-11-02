# Standalone HTTP Mode — Deployment

How to run a FastPlaz application as a long-lived HTTP server behind
nginx or Apache. Design and status: [STANDALONE-roadmap.md](STANDALONE-roadmap.md).
Working example: [examples/standalone](../examples/standalone).

## 1. Prepare the project

The program must use `fastplaz_app` instead of `fpcgi`:

```pascal
uses
  {$IFNDEF Windows}cthreads,{$ENDIF}
  fastplaz_app, sysutils, fastplaz_handler, common, ...;
```

Projects created with the FastPlaz wizard already do. For an existing
project, replace `fpcgi` with `fastplaz_app` in the `.lpr` **and** in every
unit that lists it:

```sh
grep -rlw fpcgi --include=*.pas --include=*.lpr source | xargs sed -i 's/\bfpcgi\b/fastplaz_app/'
```

Units that keep `uses fpcgi` and call `Application.Request`,
`Application.Response` or `Application.EnvironmentVariable` get fpcgi's idle
application in HTTP mode (nil request). In CGI mode both units behave the
same, so the switch can be done before deploying.

The same binary still runs as CGI when a web server starts it
(`GATEWAY_INTERFACE` is set).

## 2. Configuration

`config/config.json`:

```json
"systems": {
  "trust_proxy": true,
  "server": {
    "mode": "http",
    "address": "127.0.0.1",
    "port": 8080,
    "static": false,
    "max_requests": 10000,
    "db_persistent": false,
    "db_idle_timeout": 300
  }
}
```

| Key | Meaning |
|---|---|
| `mode` | `http` (default) or `cgi` |
| `address`, `port` | listen address; keep `127.0.0.1` behind a proxy |
| `static` | serve theme assets itself; `false` when nginx/Apache serves them |
| `max_requests` | stop after N requests (0 = never); the service manager starts a fresh process |
| `db_persistent` | keep database connections between requests (default: close after each request, as CGI) |
| `db_idle_timeout` | seconds after which a kept connection is reopened |
| `trust_proxy` (in `systems`) | take HTTPS/host/port/client IP from `X-Forwarded-*`; only behind a proxy |

Command line options override the file: `--mode`, `--port`, `--address`,
`--static=on|off`, `--max-requests=N`, `--root=<dir>`.

The working directory is the directory of the binary when it contains
`config/config.json`; use `--root` otherwise.

Ctrl+C / SIGTERM stops the server after the request in progress.

## 3. Reverse proxy

### nginx

```nginx
upstream fastplaz_app {
    server 127.0.0.1:8081;
    server 127.0.0.1:8082;      # one line per process
}

server {
    listen 443 ssl;
    server_name example.com;
    root /var/www/example/public_html;

    location /themes/ { try_files $uri =404; }   # with "static": false
    location ~ ^/(config|ztemp)/ { deny all; }

    location / {
        proxy_pass http://fastplaz_app;
        proxy_set_header Host $host;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_set_header X-Forwarded-Host $host;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    }
}
```

### Apache

```apache
<VirtualHost *:443>
    ServerName example.com
    ProxyPreserveHost On
    RequestHeader set X-Forwarded-Proto "https"
    ProxyPass        /themes/ !
    ProxyPass        / http://127.0.0.1:8080/
    ProxyPassReverse / http://127.0.0.1:8080/
    Alias /themes/ /var/www/example/public_html/themes/
</VirtualHost>
```

Modules: `mod_proxy`, `mod_proxy_http`, `mod_headers` (and
`mod_proxy_balancer` for several processes).

## 4. Keep it running

Each process handles one request at a time. Run several on different ports
for parallel requests and list them in the proxy.

### Linux (systemd)

`/etc/systemd/system/fastplaz-example@.service` (one instance per port):

```ini
[Unit]
Description=FastPlaz example on port %i
After=network.target mysql.service

[Service]
Type=simple
User=www-data
WorkingDirectory=/var/www/example/public_html
ExecStart=/var/www/example/public_html/app.bin --port=%i
Restart=always
RestartSec=1
KillSignal=SIGTERM
TimeoutStopSec=10

[Install]
WantedBy=multi-user.target
```

```sh
systemctl daemon-reload
systemctl enable --now fastplaz-example@8081 fastplaz-example@8082
journalctl -u fastplaz-example@8081 -f
```

`Restart=always` also starts a fresh process after `max_requests`.

### macOS (launchd)

`~/Library/LaunchAgents/com.example.fastplaz.plist`:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key><string>com.example.fastplaz</string>
  <key>ProgramArguments</key>
  <array>
    <string>/Users/me/www/example/public_html/app.bin</string>
    <string>--port=8081</string>
  </array>
  <key>WorkingDirectory</key><string>/Users/me/www/example/public_html</string>
  <key>KeepAlive</key><true/>
  <key>RunAtLoad</key><true/>
</dict>
</plist>
```

```sh
launchctl load ~/Library/LaunchAgents/com.example.fastplaz.plist
```

### Windows (service with NSSM)

```bat
nssm install FastPlazExample8081 C:\www\example\public_html\app.bin --port=8081
nssm set FastPlazExample8081 AppDirectory C:\www\example\public_html
nssm set FastPlazExample8081 AppExit Default Restart
nssm set FastPlazExample8081 AppStdout C:\www\example\public_html\ztemp\logs\service.log
nssm start FastPlazExample8081
```

Windows asks to allow network access on the first start unless the address
is `127.0.0.1`.

## 5. Checklist

- [ ] every `fpcgi` replaced by `fastplaz_app`
- [ ] `address` is `127.0.0.1` when a proxy is in front
- [ ] `trust_proxy` only when the app is reachable through the proxy alone
- [ ] `config/` and `ztemp/` not served by the proxy
- [ ] service restarts the process (`max_requests`, crashes)
- [ ] database: default per-request connections, or `db_persistent` with an
      idle timeout below the database server's own timeout
