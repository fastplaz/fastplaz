Use Of Template
---

syntax:
```pascal
ThemeUtil.View( TagControllerAddress, FileTemplate, Cache);
```

penggunaan:

```pascal

// default template, no cache
Response.Content:= ThemeUtil.View( @TagController);

// default template, with cache
Response.Content:= ThemeUtil.View( @TagController, '', true);

// custom template, with cache
Response.Content:= ThemeUtil.View( @TagController, 'myfolder/mytemplate', true);

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

