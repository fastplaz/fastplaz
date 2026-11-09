Use Of Template
---

syntax:
```pascal
ThemeUtil.Render( TagProcessorAddress, LayoutTemplateFile, Cache, SubModule);
```

All parameters are optional:

| Parameter | Default | Description |
|---|---|---|
| `TagProcessorAddress` | `nil` | tag handler (`TReplaceTagEvent`), ex: `@TagController` |
| `LayoutTemplateFile` | `''` | template file, relative to `themes/<theme>/templates/`; the theme extension is added when the name has none |
| `Cache` | `False` | return the cached page when available |
| `SubModule` | `False` | `True` ignores `ThemeUtil.Layout` and always uses `LayoutTemplateFile` |

When `LayoutTemplateFile` is empty, the template is taken from the active module:
`templates/modules/<module>/<act>`, then `templates/modules/<module>/master`, then `templates/master`.

Note: when `ThemeUtil.Layout` is set (and `SubModule` is `False`), it overrides `LayoutTemplateFile`.

usage:

```pascal

// default template, no cache
Response.Content:= ThemeUtil.Render;

// default template with tag controller, no cache
Response.Content:= ThemeUtil.Render( @TagController);

// default template, with cache
Response.Content:= ThemeUtil.Render( @TagController, '', true);

// custom template, with cache
Response.Content:= ThemeUtil.Render( @TagController, 'myfolder/mytemplate', true);

// render a template string instead of a file
Response.Content:= ThemeUtil.RenderFromContent( @TagController, '<h1>[$title]</h1>');

```


**Assign variable to template**

syntax:

```pascal
  ThemeUtil.AssignVar[ IndexName] := variable;
  // or
  ThemeUtil.Assign( IndexName, pointer_of_variable);
```

example

```pascal
  ThemeUtil.AssignVar['yourvariablename'] := News.Data;
  // or
  ThemeUtil.Assign('yourvariablename', @Data);
```
