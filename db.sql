CREATE TABLE IF NOT EXISTS datapoints_users (
  `id`    INTEGER PRIMARY KEY,
  `name`  TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS datapoints_groups (
  `id`      INTEGER PRIMARY KEY,
  `user_id` INTEGER NOT NULL,
  `name`    TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS datapoints (
  `id`        INTEGER PRIMARY KEY,
  `user_id`   INTEGER NOT NULL,
  `group_id`  INTEGER NOT NULL,
  `count`     INTEGER DEFAULT 0,
  `date`      DATETIME NOT NULL
);