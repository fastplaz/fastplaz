---
id: common-function
title: Common Function
sidebar_label: Common Function
---

_function_ and _procedure_ list in [FastPlaz](https://fastplaz.com).

## Converter

| Name | Description |
|---|---|
| i2s | Convert Integer to String (with execption handler) |
| s2i | Convert String to Integer  (with execption handler)  |
| f2s | Convert Float to String  (with execption handler) |
| s2f | Convert String to Float  (with execption handler) |
| b2i | Convert Boolean to Integer  |
| b2is | Convert Boolean to String (`1`/`0`)  |
| b2s | Convert Boolean to String (`True`/`False`)  |
| HexToInt | Convert Hexa string to integer  |
| MarkdownToHTML | Convert Markdown to HTML  |
| StreamToString | Convert Stream to String  |
| ISO8601ToDateTime | Convert Convert an ISO 8601 string to a TDateTime |
| StringHumanToNominal | `s := StringHumanToNominal('satu juga lima ratus dua puluh');` |
| StringHumanToDate | `TheDate := StringHumanToDate('17 agustus 1945');` |


## String 

| Name | Description |
|---|---|
| isEmpty | Determine whether a variable is empty |
| isRegex | Determine whether a variable is regex string |
| isWord | Determine whether a variable is  word |
| isURL | Determine whether a variable is url |
| isIPAddress | Determine whether a variable is ip address |
| isEmail | Determine whether a variable is email |
| isDomain | Determine whether a variable is domain |
| RandomString | Generate Random String |
| ReplaceAll | |
| StrInArray | Determine whether a String is in Array<br>See [Array Helper](/docs/helper#array_helper) for other option. |
| StringCut | |
| StringsExists | |
| StripCharsInSet | |
| StripHTML | Strip HTML Tags from a string |
| StripNumber | Strip number from a string |
| StripNonAscii | Remove Non ASCII Character |
| StripNonNumber | Strip non-number from a string |
| Strstr | Find the first occurrence of a string |
| UCStoString | |

## PHP refactor

| Name | Description |
|---|---|
| base64_encode | Encodes data with MIME base64 |
| base64_decode | Decodes data encoded with MIME base64 |
| Echo | Output a message (string/integer/double) |
| Die | Output a message and terminate the current script |
| Explode | Split a string by a string |
| file_get_contents | Reads entire file into a string |
| Implode | Join array elements with a string |
| mysql_real_escape_string | Escapes special characters in a string for use in an SQL statement |
| preg_match | Perform a regular expression match |
| preg_replace | Perform a regular expression search and replace |
| StripTags | Strip HTML and PHP tags from a string |
| ucwords | Uppercase the first character of each word in a string |
| UrlEncode | URL-encodes string |
| UrlDecode | Decodes URL-encoded string |

## Others

| Name | Description |
|---|---|
| AppendPathDelim | Add path delimiter from a path |
| DirectoryIsWritable | Determine whether a directory is writeable or not |
| DownloadFile | Download form url |
| FileCopy | |
| GetHostNameIP | Get IP from host name |
| GetUserIpAddress | Get user's current ip |
| HTMLDecode | |
| IsJsonValid | JSON string validation |
| jsonGetData | Get string from a JSON Data (TJsonData) |
| LoadCache | Read a string from cache. Cache time = 1 hour |
| LoadFromFile | Load string from a file |
| OutputJson | Output a json message and result code |
| SaveCache | Save a string to cache |
| SaveToFile | Save string to a file |
| ScanFolder | Get file list from a folder/directory |
| ZipFolder | Compress a folder to a .zip file |
