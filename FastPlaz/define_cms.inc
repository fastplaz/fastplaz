
OK = 'OK';
UNKNOWN = 'unknown';

// Defines for access levels
ACCESS_INVALID = -1;
ACCESS_NONE = 0;
ACCESS_OVERVIEW = 100;
ACCESS_READ = 200;
ACCESS_COMMENT = 300;
ACCESS_MODERATE = 400;
ACCESS_EDIT = 500;
ACCESS_ADD = 600;
ACCESS_DELETE = 700;
ACCESS_ADMIN = 800;

ERR_REDIRECT = 302;

// Group
GROUP_FIELD_ID = 'gid';
GROUP_FIELD_NAME = 'name';
GROUP_FIELD_TYPE = 'gtype';
GROUP_FIELD_DESCRIPTION = 'description';
GROUP_FIELD_PREFIX = 'prefix';
GROUP_FIELD_STATE = 'state';
GROUP_FIELD_NUMBERUSER = 'nbuser';
GROUP_FIELD_NUMBERUSER_MAX = 'nbumax';
GROUP_FIELD_LINK = 'link';
GROUP_FIELD_MASTER = 'uidmaster';

// User
USER_URL_LOGIN = 'user-login';
USER_URL_LOGOUT = 'user-loogut';
USER_URL_DASHBOARD = 'user-dashboard';
USER_URL_CHANGEPASSWORD = 'user-changepassword';
USER_URL_REGISTER = 'user-register';
USER_URL_REGISTER_THANKYOU = 'user-registerthankyou';
USER_URL_LOSTPASSWORD = 'user-lostpassword';
USER_TABLE_NAME = 'users';
USER_PRIMARY_KEY = 'uid';
USER_FIELDNAME_ID = 'uid';
USER_FIELDNAME_NAME = 'name';
USER_FIELDNAME_USERNAME = 'uname';
USER_FIELDNAME_PASSWORD = 'pass';
USER_FIELDNAME_EMAIL = 'email';
USER_FIELDNAME_ACTIVATED = 'activated';
USER_FIELDNAME_REGDATE = 'user_regdate';
USER_FIELDNAME_LASTLOGIN = 'lastlogin';


// Message
MSG_USERNAMEEMAIL_NOT_EXISTS = 'Username/Email not exists.';
MSG_ID_INVALID = 'Invalid ID';
MSG_USER_EXISTS = 'User Exists';
MSG_USER_NOTEXISTS = 'User Not exists';
MSG_USER_ADD_FAILED = 'Failed to add user';
MSG_USER_DELETE_FAILED = 'Failed delete User';
MSG_USER_ACTIVATE_FAILED = 'Failed to activate user.';
MSG_USER_DEACTIVATE_FAILED = 'Failed to deactivate user.';
MSG_USER_SUSPEND_FAILED = 'Failed to suspend user.';
MSG_USER_RESETPASSWORD_FAILED = 'Failed reset user''s password.';
MSG_GROUP_ADD_FAILED = 'Failed to add group';
MSG_GROUP_EDIT_FAILED = 'Failed to edit group';
MSG_GROUP_DELETE_FAILED = 'Failed delete group';
MSG_GROUP_NOTEXISTS = 'Group Not exists';
MSG_LOGIN_INVALID = 'Invalid UserName or Password ! <br>or your account is not active (%d times)';
MSG_TOKEN_INVALID = 'Security: Invalid CSRF Token';
MSG_PASSWORDNEW_INVALID = 'Invalid New Password';
MSG_PASSWORD_INVALID = 'Invalid Password';
MSG_PASSWORD_UPDATED = 'Your password updated.';
MSG_PASSWORD_FAILED = 'Failed change password';
MSG_FIELD_INCORRECT = 'Please insert field correctly';
MSG_VALUE_INVALID = 'Invalid Value';
MSG_NOPERMISSION = 'No Permission';
MSG_NOLOGIN = 'No Logged-in';
MSG_NOLOGIN_OR_SESSIONEXPIRED = 'No Logged-in or session expired.';
MSG_PENDINGUSER = 'You have %d pending user(s)';
MSG_DEMO_NORESETPASSWORD = 'Dont reset admin''s password at demo version.';
MSG_DEMO_FAILEDDELETEUSERADMIN = 'Failed delete admin user';
MSG_DEMO_FAILEDSUSPEND = 'Failed suspend admin user';


