
FastPlaz
===
**[FastPlaz](http://www.fastplaz.com)** adalah satu satu web framework dengan menggunakan bahasa  free pascal  . Dibuat sedemikian rupa sehingga requirement server yang diperlukan pun sederhana dan minimal, dan bahkan bisa di  compile  langsung dari console/terminal tanpa memerlukan ide/editor  Lazarus  . Cukup dengan menggunakan  apache  biasa seperti di shared hosting, fastplaz sudah bisa langsung digunakan.


Package
---
* fastplaz_cms.lpk


## USER UTILS USAGE
---

initialization
```
  UsersUtil := TUsersUtil.Create();
```
or with this statement
```
  with TUsersUtil.Create() do
  begin
  .
  .
  .
  end;
```

check is loggedin or not
```
  if UsersUtil.isLoggedIn then
  begin
    ....
  end
```

login with username & password
```
  if UsersUtil.Login( 'theusername', 'thepassword') then
  begin
  ....
  end;
```

add user
```
  lastUserID := UsersUtil.Add( 'theusername', 'theusername@email.com', 'password');

  // or, with random password

  lastUserID := UsersUtil.Add( 'theusername', 'theusername@email.com');
```

add user with params
```
  params := TStringList.Create;
  params.Values['activated'] := '1';
  params.Values['approved_by'] := '0';
  lastUserID := UsersUtil.Add( 'theusername', 'theusername@email.com', '', params);
```

assign user to group
```
  UsersUtil.AssignToGroup( 24, 1);

  UsersUtil.AssignToGroup( 24, 'Users');
```

change password
```
  UsersUtil.ChangePassword( 3, 'newpassword');
```

## GROUP UTILS USAGE
---

add group
```
  with TGroupsUtil.Create() do
  begin
    Add( 'groupname', 'group description', GROUP_TYPE_PUBLIC)
    ;
  end;
```

add user to group
```
  TGroupsUtil.AddUserToGroup( 1, 'Users');

  // or

  TGroupsUtil.AddUserToGroup( 1, 1);
```

## PERMISSION UTILS
---

check permission
```
   with TPermissionUtil.Create() do
   begin
     if checkPermission( 'modulename', 'instance', ACCESS_DELETE, UserId) then
     begin
     .
     .
     .
     end;
     Free;
   end;
```

other way to check permission from existing loggedin user
```
  if UsersUtil.checkPermission( 'modulename', 'instance', ACCESS_DELETE) then
  begin
  .
  .
  .
  end;
```

get security level
```
  userID := 2;
  secLevel := getSecurityLevel( userID, 'modulname', 'list');
```

get security level from any instance
```
  userID := 2;
  secLevel := getSecurityLevel( userID, 'modulname', 'any');
```
