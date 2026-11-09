# FastPlaz reference for coding agents

FastPlaz is a web framework for Free Pascal (FPC 3.2.2, `{$mode objfpc}{$H+}`).
An application compiles to one native binary that runs as a standalone HTTP
server or as CGI. This text is served by the Lazarus IDE (AI Agent Plane) as
the MCP resource `fastplaz://reference` and by the tool
`get_fastplaz_reference` (argument `section` returns one part).

Free Pascal is not Delphi: generics need `specialize`, method pointers need
`@` (`Tags['x'] := @Handler`), strings are 1-based.

## Project layout

```
myapp/
├── source/                    Lazarus project
│   ├── myapp.lpi / myapp.lpr  program file
│   ├── routes.pas             Route[...] registrations (optional)
│   └── controllers/           one unit per controller
└── public_html/               web root
    ├── myapp.bin              the compiled binary (output file of the .lpi)
    ├── config/config.json     configuration
    ├── themes/default/        theme: templates/*.html, style/, js/
    └── ztemp/                 cache, logs, sessions (must be writable)
```

The program file uses `fastplaz_app` (standalone + CGI in one binary, the
default for new projects) or `fpcgi` (CGI only), plus `fastplaz_handler`,
`common`, the controller units and `routes`. Use the IDE tools
`project_info` and `list_project_units` to see the real layout of the open
project; older projects keep controllers next to the `.lpr`.

Run standalone: start the binary, open http://127.0.0.1:17845
(`--port=`, `--address=`, `--mode=http|cgi`; config `systems/server`).
In CGI mode Apache starts it through `.htaccess` rewrite rules.

## Controllers

Create a new controller with the tools `create_simple_controller` (HTML page
rendered through the theme) or `create_api_controller` (JSON), then edit it.
In FastPlaz a "module" is a controller.

```pascal
unit product_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, html_lib, fastplaz_app, fpjson, json_lib, HTTPDefs,
  fastplaz_handler, database_lib, dateutils, string_helpers, datetime_helpers,
  array_helpers, json_helpers;

type
  TProductController = class(TMyCustomController)
  private
    function Tag_MainContent_Handler(const TagName: string; Params: TStringList): string;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;
    procedure Get; override;
    procedure Post; override;
  end;

implementation

uses theme_controller, common;

constructor TProductController.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TProductController.Destroy;
begin
  inherited Destroy;
end;

procedure TProductController.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  // runs before Get/Post; DataBaseInit here when the controller uses models
end;

procedure TProductController.Get;
begin
  Tags['maincontent'] := @Tag_MainContent_Handler;
  Response.Content := ThemeUtil.Render();
end;

procedure TProductController.Post;
begin
  Response.Content := 'This is POST Method';
end;

function TProductController.Tag_MainContent_Handler(const TagName: string;
  Params: TStringList): string;
begin
  Result := H3('Products');
end;

initialization
  Route['product'] := TProductController;

end.
```

- Override the methods you answer: `Get`, `Post`, `Put`, `Patch`,
  `Delete`, `Options`. A method that is not overridden answers a placeholder
  text ("Default GET Result"), so override every method the route accepts.
- Output: `Response.Content := ...`, `Response.ContentType := 'application/json'`,
  `Response.Code := 404` (fpweb `TResponse`).
- `Redirect(URL, FlashMessage)` (unit `common`) sends a redirect with an
  optional flash message; `Die(Message, Code)` writes and ends the request;
  `OutputJson(Code, Message)` answers `{"code":..,"msg":..}` and ends it.
- `EnableCors(Origin)` adds CORS headers (default `*`).
- `TMyCustomController` is `TMyCustomWebModule` (unit `fastplaz_handler`).

## Routes

Register a controller class under a name, in the controller's
`initialization` section or in `routes.pas` (a `uses` of the controller
unit plus the registrations in its `initialization`). When the project has
`routes.pas`, put new routes there.

```pascal
initialization
  Route['product'] := TProductController;              // /product
  Route['/'] := TMainController;                         // home page
  Route.Add('product-detail', '^/product/(?P<id>[0-9]+)$',
    TProductDetailController, 'GET');                    // regex route
  Route.Add('docs', TDocsModule, 'GET');                 // one method only
```

- `Route.Add(Name, Pattern, Class, Method)`: `Pattern` is a regular
  expression matched against the path (with a leading `/`). It is not
  anchored: use `^...$` unless a prefix match is wanted. Named groups become
  request variables (`_GET['id']`), numbered groups `_GET['$1']`, `_GET['$2']`.
- `Method` is `''` (every method) or one method (`'GET'`, `'POST'`, ...);
  other methods then get `405 Method Not Allowed`.
- The module name (`Route['name']`) is also the first path segment: `/name`.

## Request, session and headers

Globals from `fastplaz_handler`:

| Name | What |
|---|---|
| `_GET['name']` | query string and route variables (string) |
| `_POST['name']` | form fields (string) |
| `_REQUEST['name']` | `_GET`, then `_POST` |
| `_SERVER['REMOTE_ADDR']` | CGI variables |
| `_SESSION['key']` | session value (Variant); `SessionController['key']` reads/writes strings |

Controller properties: `Header['Authorization']` (request header),
`CustomHeader['X-Name'] := value` (response header), `isPost`, `isGet`,
`isPut`, `isPatch`, `isDelete`, `URI`, `BaseURL`, `Files` (uploads),
`SessionID`, `FlashMessages := 'text'`, `isValidCSRF`.

Request bodies of JSON APIs: `Request.Content` (fpweb), parse with
`TJSONUtil.LoadFromJsonString` or `GetJSON` (unit `fpjson`, `jsonparser`).

Always encode user input written into HTML: `HTMLEncode(s)` (unit `html_lib`).

## JSON (TJSONUtil)

Unit `json_lib`. Paths use `/`.

```pascal
var
  json: TJSONUtil;
begin
  json := TJSONUtil.Create;
  try
    json['code'] := Int16(0);
    json['msg'] := 'OK';
    json['data/user/name'] := _GET['name'];
    Response.ContentType := 'application/json';
    Response.Content := json.AsJSON;      // AsJSONFormated: indented
  finally
    json.Free;
  end;
end;
```

- Reading: `json.LoadFromJsonString(text)` (False on invalid JSON), then
  `json['data/user/name']` (Variant) or `json.Item[path]`,
  `json.ValueArray[path]` (TJSONArray). `json.Data` is the root TJSONObject.
- A TSimpleModel result as JSON: `Model.AsJsonArray` (caller frees it, or
  add it to a TJSONObject that owns it).
- The generated code assigns numbers with a type cast (`Int16(0)`,
  `Int64(x)`) so the Variant, and the JSON value, has the intended type.

## Themes and templates

Theme files live in `public_html/themes/<theme>/templates/`. `master.html`
is the default layout; `ThemeUtil.Layout := 'home'` (before `Render`) or
`ThemeUtil.Render(nil, 'home')` uses `home.html`.

```pascal
Tags['maincontent'] := @Tag_MainContent_Handler;  // [maincontent]
ThemeUtil.Assign('$pagetitle', 'Products');         // [$pagetitle]
ThemeUtil.Assign('products', Product);              // a TSimpleModel (or TSQLQuery) for foreach
Response.Content := ThemeUtil.Render();
```

Tag handler signature: `function(const TagName: string; Params: TStringList): string`.
Tag parameters from the template (`[product id=5 format=short]`) are in
`Params.Values['id']`.

Template tags (square brackets):

| Tag | Output |
|---|---|
| `[maincontent]` | the handler registered with `Tags['maincontent']` |
| `[$name]` / `[name]` | a string assigned with `ThemeUtil.Assign('$name', 'value')` (the key as written) |
| `[$baseurl]`, `[$themefullpath]`, `[sitename]`, `[$slogan]` | site values from config.json |
| `[flashmessages]` | pending flash messages (HTML encoded) |
| `[include file=parts/header.html]` | another template file |
| `[config key=systems/sitename]` | a config value |
| `[csrf-token]` | a CSRF hidden input |
| `[assign var=x value=y]`, `[value var=x]` | template variables |
| `[!anything]` | ignored; `[*comment]` becomes an HTML comment |

Loops and conditions:

```html
[foreach type=dataset from=products item=p]
  <li>[p index=name] - [p index=price]</li>
[/foreach from=products]

[if $role = "admin"]<a href="admin">Admin</a>[else]Welcome[/if]
```

- The closing tag needs `from=`: `[/foreach from=products]`. A bare
  `[/foreach]` never matches.
- `type` is `dataset` (a model or `TSQLQuery` given to `ThemeUtil.Assign`),
  `table` or `json` (`[p index=a.b]` for nested JSON fields).
- In `[if a = b]` an unquoted right side is a variable; quote literals.

## Database models (TSimpleModel)

Unit `database_lib`. Create a model unit with the tool `create_model`.

```pascal
type
  TProductModel = class(TSimpleModel)
  public
    constructor Create(const DefaultTableName: string = '');
  end;

constructor TProductModel.Create(const DefaultTableName: string = '');
begin
  inherited Create(DefaultTableName); // table "products"
end;
```

Naming: the table comes from the class name: `TProductModel` -> `products`,
`TUserGroupModel` -> `user_groups` (plus the table prefix from config). The
primary key is the capitals + `id`: `TUserModel` -> `uid`, `TUserGroupModel`
-> `ugid`. Pass names explicitly when the schema differs:
`inherited Create('tbl_product', 'id')`.

Connect first: `DataBaseInit;` (once per request, e.g. in
`BeforeRequestHandler`); it reads `database/default` from config.json.

```pascal
var
  Product: TProductModel;
begin
  DataBaseInit;
  Product := TProductModel.Create();
  try
    // query builder
    if Product.Select('id, name, price')
      .Where('price > %d', [1000])
      .OrderBy('name')
      .Limit(10)
      .Open then
    begin
      while not Product.EOF do
      begin
        Result := Result + HTMLEncode(Product['name']) + '<br>';
        Product.Next;
      end;
    end;

    // find by primary key / conditions
    if Product.Find(5) then ...
    if Product.Find(['category_id=3', 'active=1'], 'name', 20) then ...

    // insert
    Product.New;
    Product['name'] := 'Book';
    Product['price'] := 25000;
    if Product.Save then
      newId := Product.LastInsertID;

    // update: Save with a WHERE condition (raw SQL, quote values)
    Product.New;
    Product['price'] := 30000;
    Product.Save('id=5');

    Product.Delete(5);
  finally
    Product.Free;
  end;
end;
```

- Field values: `Model['field']` (Variant), `Model.Data` (the `TSQLQuery`,
  `Model.Data.FieldByName('x').AsString`), `RecordCount`, `EOF`, `Next`.
- `Where` with a format string escapes nothing: quote string values with
  `Model.QuoteValue(s)` (or use `ParamByName`), never paste request input
  into SQL.
- Other: `All`, `FindFirst`, `OrWhere`, `GroupBy`, `AddJoin`/`AddLeftJoin`,
  `Exec(sql)`, `StartTransaction`/`Commit`/`Rollback`, `UseSoftDelete`.
- `driver` in config.json is the SQLdb connector name: `MySQL 8.0`,
  `MySQL 5.7` (other MySQL versions alike), `PostgreSQL`, `SQLite3`,
  `Firebird`.

## Configuration (config.json)

`public_html/config/config.json`:

```json
{
  "systems": {
    "sitename": "My App",
    "baseurl": "",
    "debug": false,
    "theme": "default",
    "theme_enable": true,
    "module_default": "main",
    "language_default": "en",
    "temp": "ztemp",
    "server": { "mode": "http", "port": 17845, "address": "127.0.0.1" }
  },
  "database": {
    "default": {
      "driver": "MySQL 8.0",
      "hostname": "localhost",
      "port": "",
      "username": "root",
      "password": "",
      "database_name": "mydb",
      "prefix": "",
      "charset": ""
    }
  }
}
```

`charset` empty means `utf8mb4` on MySQL 5.5+ (`utf8` on MySQL 4.x - 5.1,
the library default on other drivers); `"none"` keeps the server default.
No `SET NAMES` / `SET CHARACTER SET` query is needed after `DataBaseInit`.

Read values with `Config.GetValue('systems/sitename', 'default')` or
`Config['your/key']` (global `Config`, unit `common`). Constants such as
`_SYSTEM_SITENAME` hold the standard keys. Add your own sections for app
settings instead of hard-coding them. Never write passwords into source code.

## Helpers

Unit `common` (and `fastplaz_handler`):

- Conversion: `i2s`, `s2i`, `f2s`, `s2f`, `b2s`, `s2b` (no exceptions on bad input).
- Strings: `Explode(s, delim)` (TStrings, caller frees), `Implode`,
  `isEmpty`, `isEmail`, `isURL`, `RandomString`, `StripHTML`, `UrlEncode`,
  `UrlDecode`, `base64_encode`, `base64_decode`, `preg_match`, `preg_replace`.
- Files: `LoadFromFile`, `SaveToFile`, `file_get_contents`, `ScanFolder`.
- HTML (`html_lib`): `H1`..`H6` (e.g. `H3('Title')`), `HTMLEncode`,
  `HTMLUtil.Link`, `HTMLUtil.CSRF`.
- Logging: `LogUtil.Add(message, 'tag')` (unit `logutil_lib`) writes to
  `ztemp/logs`.
- Type helpers (`string_helpers`, `integer_helpers`, `datetime_helpers`,
  `json_helpers`): e.g. `s.IsEmpty`, `s.AsInteger`, `dt.HumanReadable`; see
  `docs/string-helper.md` and `docs/datetime-helper.md`.
- Integrations live in `FastPlaz/src/integration` (OpenAI, Telegram, ...);
  read the unit before using one.

## Workflow in the IDE

1. `project_info`, `list_project_units`: where the controllers, `routes.pas`
   and the output binary are.
2. Generate with `create_simple_controller`, `create_api_controller`,
   `create_model`; edit the generated unit.
3. `compile_project`; fix the errors it returns (file, line, column), then
   compile again until it succeeds.
4. Run the binary (standalone) and request the route to check the output.

## Pitfalls

- Free what you create (`TJSONUtil`, models, `Explode` results) in
  `try ... finally`; a CGI process ends, a standalone server keeps leaking.
- Globals and class vars live as long as the process: in standalone mode one
  process serves many requests, so do not keep per-request state in them.
- A new controller unit must be in the program's `uses` (directly or through
  `routes.pas`) or its `initialization` never runs and the route answers 404.
- A unit not found at compile time: the directory must be in the project's
  unit search path (`project_info` lists it).
- Template files are read at run time from `public_html/themes`; the binary
  needs no rebuild after a template change, but does after Pascal changes.
- Apache drops request headers with underscores; use dashes.
