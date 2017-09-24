CREATE TABLE `articles` (
  `id`      BIGINT        NOT NULL                  AUTO_INCREMENT,
  `header`  VARCHAR(500) NOT NULL,
  `preview` TEXT          NOT NULL,
  `body`    TEXT          NOT NULL,
  `meta`    JSON          NOT NULL,
  PRIMARY KEY (`id`)
)
  DEFAULT CHARSET = utf8;