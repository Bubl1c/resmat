CREATE TABLE `test_set_confs` (
  `id`               BIGINT       NOT NULL                  AUTO_INCREMENT,
  `name`             VARCHAR(500) NOT NULL,
  `max_tests_amount` INT(11)      NOT NULL,
  PRIMARY KEY (`id`)
)
  DEFAULT CHARSET = utf8;

CREATE TABLE `test_group_confs` (
  `id`   BIGINT       NOT NULL                  AUTO_INCREMENT,
  `name` VARCHAR(500) NOT NULL,
  PRIMARY KEY (`id`)
)
  DEFAULT CHARSET = utf8;

CREATE TABLE `test_set_conf_test_group_confs` (
  `id`                  BIGINT  NOT NULL                  AUTO_INCREMENT,
  `test_set_conf_id`    BIGINT  NOT NULL,
  `test_group_conf_id`  BIGINT  NOT NULL,
  `proportion_percents` TINYINT NOT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (test_set_conf_id)
  REFERENCES test_set_confs (id)
    ON DELETE CASCADE,
  FOREIGN KEY (test_group_conf_id)
  REFERENCES test_group_confs (id)
    ON DELETE CASCADE
)
  DEFAULT CHARSET = utf8;

CREATE TABLE `test_confs` (
  `id`            BIGINT        NOT NULL                  AUTO_INCREMENT,
  `group_conf_id` BIGINT        NOT NULL,
  `question`      VARCHAR(2000) NOT NULL,
  `image_url`      VARCHAR(1000) NULL,
  `options`       JSON          NOT NULL,
  `test_type`     SMALLINT      NOT NULL,
  `help`          VARCHAR(255)  NULL                      DEFAULT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (group_conf_id)
  REFERENCES test_group_confs (id)
    ON DELETE CASCADE
)
  DEFAULT CHARSET = utf8;

CREATE TABLE `user_exam_step_attempt_test_sets` (
  `id`               BIGINT NOT NULL                  AUTO_INCREMENT,
  `step_attempt_id`  BIGINT NOT NULL,
  `test_set_conf_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (step_attempt_id)
  REFERENCES user_exam_step_attempts (id)
    ON DELETE CASCADE,
  FOREIGN KEY (test_set_conf_id)
  REFERENCES test_set_confs (id)
    ON DELETE RESTRICT
)
  DEFAULT CHARSET = utf8;

CREATE TABLE `user_exam_step_attempt_test_set_tests` (
  `id`                       BIGINT     NOT NULL                  AUTO_INCREMENT,
  `step_attempt_test_set_id` BIGINT     NOT NULL,
  `test_conf_id`             BIGINT     NOT NULL,
  `done`                     TINYINT(1) NOT NULL,
  `mistakes`                 SMALLINT   NOT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (step_attempt_test_set_id)
  REFERENCES user_exam_step_attempt_test_sets (id)
    ON DELETE CASCADE,
  FOREIGN KEY (test_conf_id)
  REFERENCES test_confs (id)
    ON DELETE RESTRICT
)
  DEFAULT CHARSET = utf8;

