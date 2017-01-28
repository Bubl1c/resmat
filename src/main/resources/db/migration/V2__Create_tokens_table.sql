CREATE TABLE `tokens` (
  `id`      BIGINT NOT NULL AUTO_INCREMENT,
  `user_id` BIGINT NOT NULL,
  `token`   VARCHAR(50) NOT NULL,
  `created` datetime NOT NULL,
  `expires` datetime,
  PRIMARY KEY (`id`),
  INDEX user_id_ind (user_id),
    FOREIGN KEY (user_id)
        REFERENCES users(id)
        ON UPDATE RESTRICT ON DELETE CASCADE
) DEFAULT CHARSET=utf8;