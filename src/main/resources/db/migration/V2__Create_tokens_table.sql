CREATE TABLE `tokens` (
  `id`      BIGINT      NOT NULL AUTO_INCREMENT,
  `user_id` BIGINT      NOT NULL,
  `token`   VARCHAR(50) NOT NULL,
  `created` DATETIME    NOT NULL,
  `expires` DATETIME,
  PRIMARY KEY (`id`),
  FOREIGN KEY (user_id)
  REFERENCES users(id)
    ON UPDATE RESTRICT
    ON DELETE CASCADE
)
  DEFAULT CHARSET = utf8;