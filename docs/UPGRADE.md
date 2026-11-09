# Upgrade notes – review and fix pass (November 2026)

This release fixes many bugs found in a full review of `FastPlaz/src` and `tools/src`. Most fixes are invisible to applications. The changes below can change how an existing application behaves; check them before upgrading. Every item names the setting or call to look at.

## Must check

### Redirect and die do not stop your code

`Redirect()`, `die()` and `DieRaise()` send the response and flag the application for termination. They do **not** leave the current method. Code after the call still runs, and anything it writes is lost or, worse, a database write still happens.

```pascal
// before: the delete ran even for a user without permission
if not isAllowed then
  Redirect(BaseURL + 'login');
Model.Delete(ID);

// now
if not isAllowed then
begin
  Redirect(BaseURL + 'login');
  Exit;
end;
Model.Delete(ID);
```

The framework, the cms module and the generated controllers now exit after every redirect. Check your own controllers for the same pattern.

### Session timeout is in minutes

`systems/session_timeout` and `SessionController.TimeOut` are minutes, as documented. Before, the value was compared against seconds, so the default of 3 days (`4320`) expired after 72 minutes. If you worked around this by setting a very large value, lower it again.

### Session directory

When `systems/session_dir` is empty, session files go to `<TEMP>/sessions`. `TEMP` is the environment variable, so a custom `TEMP` is honoured. `ztemp/sessions` under the application is only the fallback when `TEMP` is not set or not writable. Set `systems/session_dir` if several applications share one host and must not share sessions.

### Failed Save and Delete return False

`TSimpleModel.Save`, `Delete` and `SoftDelete` return `False` and fill `Message` when the statement fails. Before, a failure ended the request with an error page. Check the result where a failure matters:

```pascal
if not Model.Save('id=' + Model.QuoteValue(id)) then
begin
  LogUtil.Add(Model.Message, 'model');
  Exit;
end;
```

### Flash messages are HTML encoded

`[flashmessages]` encodes every message. A message that intentionally contains markup is shown as text. Use `[flashmessages html=1]` for trusted messages only.

### Client IP and proxy headers

`X-Forwarded-For` and `Client-IP` are ignored by default because any client can set them. Behind a reverse proxy, set:

```json
"systems" : { "trust_proxy" : true }
```

### reCAPTCHA

The v1 API was shut down by Google. `TReCaptcha` only talks to `siteverify`, and `THTMLUtil.ReCaptcha()` always renders the v2 widget regardless of its `Version` argument.

### Deprecated integrations

The services behind these units are discontinued. The classes still compile but are marked `deprecated` and will be removed later: `bmkg`, `clarifai`, `googleanalytics` (Universal Analytics), `kloudlesscalendar`, `newrelic`, `office365`, `yandextranslate`, `zomato`.

## Good to know

### Database

- `TSimpleModel.QuoteIdentifier()` and `QuotedTableName` quote table and alias names in the generated SQL, so names such as `order` or `my-table` work.
- `TSimpleModel.QuoteValue()` quotes a value for the active driver. Use it for every value you put into a `Where` string.
- `SoftDelete` is implemented. With `UseSoftDelete`, `Delete` marks rows through `SoftDeleteField` (default `is_deleted`).
- `TSimpleModel.Create('')` derives the table from the class name: `TUserGroupModel` becomes `user_groups`.

### Theme

- New filter `escape` (aliases `html`, `htmlspecialchars`) HTML-encodes a value: `[$name|escape]`.
- `[if]` supports `=`, `==`, `!=`, `<>`, `<`, `>`, `<=`, `>=`, nesting, multi-line HTML and quoted values.
- `[foreach]` blocks can be nested, and a dataset is rewound before it is rendered.
- Template names that come from the request go through `TThemeUtil.SafeTemplateName()`; `..` and path separators are removed.
- Include paths that start with `/` are resolved from the web root, not from the drive root.
- The page cache is kept per session when the session holds data, so one visitor no longer sees another visitor's cached page.

### Security

- Forms get one CSRF token per form id, so two forms on one page no longer invalidate each other. Check with `isValidCSRF` in the controller, render with `HTMLUtil.CSRF('formid')`.
- Session cookies are `HttpOnly`, `SameSite=Lax`, and `Secure` when the request came over HTTPS.
- `SessionController.RegenerateID` issues a new session id, for example after login.
- Header values set through the handler have line breaks removed.
- `RandomString()` uses a cryptographically secure generator.
- A request with a method the controller does not handle gets `405 Method Not Allowed`.
- `/initialize` refuses to run when `config/config.json` already exists.
- `EnableCors` no longer combines `*` with credentials; pass an explicit origin to allow credentials.

### Libraries

- `TJSONUtil` accepts an array as the root document again.
- `TMyConfig.ValidateFile` returns `True` for a valid file; `SetDataValue` replaces the node and copies the value, the caller keeps ownership.
- `TMailer` sends Cc and Bcc, and works without Synapse through the built-in `smtp_lib` client. `HeloName` defaults to the sender's domain.
- `TRedisLib` supports IPv6 and applies `TimeOut` to connecting. `Synch` is deprecated and does nothing (`SYNCH` is not a Redis command).
- `TTelegramIntegration.UpdatesSynchronize` decides whether the update thread uses `Synchronize`; it is off by default, so the thread also finishes in console and CGI programs.
- `DateTimeHuman()` parses dates independently of the locale.

### IDE tools

- New module, controller, model and project names are turned into valid Pascal identifiers (`my-app` becomes `myapp`, `2fa` becomes `_2fa`).
- Cancelling the controller or model wizard no longer creates a file.
- *Create Theme Structure* copies the default theme again.
- The Database Explorer uses the configured port and keeps the table prefix when a connection is edited.
- The unfinished Swagger project generator was removed.

## Tests

Every library unit now has an FPCUnit suite. See [UNIT_TEST.md](../UNIT_TEST.md) for how to run them.
