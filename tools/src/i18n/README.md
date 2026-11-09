# Translations

`fastplaz_tools` and `fastplaz_runtime` write their resource strings to this folder (*Package Options → i18n*).

- `<unit>.pot` is the template. Lazarus regenerates it on every package build; do not edit it by hand.
- `<unit>.<language>.po` is a translation, for example `de_common.id.po`. The IDE loads it automatically when its own language is `<language>`. A `.po` file without a language code is never loaded.

To add or update a translation, copy the new entries from the `.pot` into the `.po` file of your language and fill `msgstr`. An empty `msgstr` keeps the English text.

Keep these entries untranslated:

| Unit | Identifier | Reason |
| --- | --- | --- |
| `de_common` | `rs_fast_code_mark` (`//`) | marker the Fast Code command looks for |
| `de_common` | `rs_fast_code` | also the IDE command name, which stores its shortcut |
| `de_common` | `rsjsonsnull` (`null`) | JSON literal |

The confirmations `rsaskcanceloperation` and `rsaskcanceldangeroperation` are phrased so that **Yes cancels**. Keep that meaning in every language, so an accidental Enter never deletes anything.
