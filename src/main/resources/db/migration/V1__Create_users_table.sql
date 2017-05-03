CREATE TABLE `student_groups` (
  `id`   BIGINT       NOT NULL AUTO_INCREMENT,
  `name` VARCHAR(100) NOT NULL UNIQUE,
  PRIMARY KEY (`id`)
)
  DEFAULT CHARSET = utf8;

CREATE TABLE `users` (
  `id`         BIGINT       NOT NULL AUTO_INCREMENT,
  `username`   VARCHAR(100) NOT NULL UNIQUE,
  `password`   VARCHAR(100) NOT NULL,
  `first_name` VARCHAR(255) NOT NULL,
  `last_name`  VARCHAR(255) NOT NULL,
  `email`      VARCHAR(255) NOT NULL,
  `user_type`  SMALLINT     NOT NULL,
  `group_id`   BIGINT       NULL,
  `access_key` VARCHAR(100) NOT NULL UNIQUE,
  PRIMARY KEY (`id`),
  FOREIGN KEY (group_id)
  REFERENCES student_groups(id)
    ON UPDATE RESTRICT
    ON DELETE CASCADE
)
  DEFAULT CHARSET = utf8;