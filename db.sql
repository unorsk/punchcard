CREATE TABLE `datapoints_users` (
  `id`    INT NOT NULL AUTO_INCREMENT,
  `name`  TEXT NOT NULL,
  PRIMARY KEY (`id`)
);

CREATE TABLE `datapoints_groups` (
  `id`      INT NOT NULL AUTO_INCREMENT,
  `user_id` INT NOT NULL,
  `name`    TEXT NOT NULL,
  PRIMARY KEY (`id`)
);

CREATE TABLE `datapoints` (
  `id`        INT NOT NULL AUTO_INCREMENT,
  `user_id`   INT NOT NULL, -- so that I don't need to join with the users table (:
  `group_id`  INT NOT NULL,
  `count`     INT DEFAULT 0,
  `date`      DATE NOT NULL,
  PRIMARY KEY (`id`)
);