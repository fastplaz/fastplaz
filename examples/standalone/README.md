# FastPlaz Standalone Example

One binary that runs either as a **standalone HTTP server** or as **CGI**.
See [docs/STANDALONE-roadmap.md](../../docs/STANDALONE-roadmap.md) for the
design.

```
standalone/
├── source/                 Lazarus project (standalone.lpi)
│   ├── standalone.lpr      uses fastplaz_app instead of fpcgi
│   ├── routes.pas
│   └── controllers/
└── public_html/            web root: binary, config/, themes/, ztemp/
```

Routes:

| URL | What it shows |
|---|---|
| `/` | requests handled by the process, session visit counter, `?name=` |
| `/go` | redirect with a flash message (the server keeps running) |
| `/info` | the CGI variables as the app sees them, as JSON |

## Build

```
lazbuild source/standalone.lpi
```

The binary is written to `public_html/standalone.bin`.

## Run standalone

```
public_html/standalone.bin
```

Open http://127.0.0.1:8080. The working directory is switched to the
binary's directory when `config/config.json` is found there, so it can be
started from anywhere.

Options (command line wins over `config/config.json` → `systems/server`):

| Option | Config key | Default |
|---|---|---|
| `--mode=http\|cgi` | `mode` | `http` (`cgi` when started by a web server) |
| `--port=8080` | `port` | `8080` |
| `--address=127.0.0.1` | `address` | `127.0.0.1` |
| `--static=on\|off` | `static` | `on` |
| `--max-requests=N` | `max_requests` | `0` (never); stop after N requests so a service manager starts a fresh process |
| `--root=<dir>` | – | directory of the binary (when it holds `config/config.json`) |

`static` serves theme assets (css, js, images, fonts, ...) directly. Turn it
off when nginx/Apache serves them. HTML templates, JSON, binaries, dot files
and `ztemp/` are never served as files.

Run several processes on different ports for more throughput; each one
handles one request at a time.

Ctrl+C (or SIGTERM on Linux/macOS) stops the server after the request in
progress; a second Ctrl+C ends it at once.

## Behind nginx

```nginx
server {
    listen 80;
    server_name example.com;
    root /var/www/example/public_html;

    # optional: let nginx serve the assets (then use --static=off)
    location /themes/ { try_files $uri =404; }
    location /ztemp/  { deny all; }
    location /config/ { deny all; }

    location / {
        proxy_pass http://127.0.0.1:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    }
}
```

Several processes:

```nginx
upstream fastplaz {
    server 127.0.0.1:8081;
    server 127.0.0.1:8082;
}
# ... proxy_pass http://fastplaz;
```

## Behind Apache

```apache
<VirtualHost *:80>
    ServerName example.com
    ProxyPreserveHost On
    ProxyPass        / http://127.0.0.1:8080/
    ProxyPassReverse / http://127.0.0.1:8080/
    RequestHeader set X-Forwarded-Proto "http"
</VirtualHost>
```

(needs `mod_proxy`, `mod_proxy_http`, `mod_headers`)

## Run as CGI

Unchanged from a classic FastPlaz app: point Apache at `public_html` (the
included `.htaccess` sends every request to `standalone.bin`). The web
server sets `GATEWAY_INTERFACE`, so the binary runs as CGI without any
option.

## Not yet done

Phase 2 of the roadmap: `X-Forwarded-*` handling behind a proxy, memory
audit for long runs, database reconnect, service files.
