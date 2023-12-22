# AJAX

Apache 2.4 now drops all headers with underscores, for 'security' reasons. Where Accept.Encoding and Accept-Encoding is the header name. Underscores are replaced with dots and hyphens respectively.

```
SetEnvIfNoCase ^Accept.Encoding$ ^(.*)$ fix_accept_encoding=$1
RequestHeader set Accept-Encoding %{fix_accept_encoding}e env=fix_accept_encoding
```

or

```
SetEnvIfNoCase ^X.REQUESTED.WITH$ ^(.*)$ fix_accept_encoding=$1 
RequestHeader set X-REQUESTED-WITH %{fix_accept_encoding}e env=fix_accept_encoding
```

## References
-  [Use crumbs to protect your Ajax calls from Cross-site request forgery (CSRF/XSRF)](https://abhinavsingh.com/web-security-using-crumbs-to-protect-your-php-api-ajax-call-from-cross-site-request-forgery-csrfxsrf-and-other-vulnerabilities/)



