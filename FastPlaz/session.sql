DROP TABLE IF EXISTS session_info;
CREATE TABLE session_info (
  sessid varchar(222) NOT NULL DEFAULT '',
  ipaddr varchar(32) DEFAULT NULL,
  lastused bigint(20) DEFAULT 0,
  uid bigint(20) DEFAULT 0,
  remember tinyint(4) DEFAULT 0,
  vars longtext,
  PRIMARY KEY (sessid)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

