CREATE TABLE `exam_confs` (
  `id`          BIGINT       NOT NULL                  AUTO_INCREMENT,
  `name`        VARCHAR(100) NOT NULL,
  `description` TEXT         NOT NULL,
  `max_score`   SMALLINT     NOT NULL,
  PRIMARY KEY (`id`)
)
  DEFAULT CHARSET = utf8;

CREATE TABLE `exam_step_confs` (
  `id`                         BIGINT       NOT NULL AUTO_INCREMENT,
  `exam_conf_id`               BIGINT       NOT NULL,
  `sequence`                   SMALLINT     NOT NULL,
  `name`                       VARCHAR(100) NOT NULL,
  `step_type`                  SMALLINT     NOT NULL,
  `mistakes_per_attempt_limit` SMALLINT     NOT NULL,
  `mistake_value_percents`     TINYINT      NOT NULL,
  `attempts_limit`             SMALLINT     NOT NULL,
  `attempt_value_percents`     TINYINT      NOT NULL,
  `max_score`                  SMALLINT     NOT NULL,
  `data_set`                   JSON                  DEFAULT NULL,
  `has_to_be_submitted`        TINYINT(1)   NOT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (exam_conf_id)
  REFERENCES exam_confs (id)
    ON DELETE CASCADE
)
  DEFAULT CHARSET = utf8;

