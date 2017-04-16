CREATE TABLE `exam_confs` (
  `id`          BIGINT       NOT NULL                  AUTO_INCREMENT,
  `name`        VARCHAR(100) NOT NULL,
  `description` TEXT         NOT NULL                  DEFAULT '',
  PRIMARY KEY (`id`)
)
  DEFAULT CHARSET = utf8;

CREATE TABLE `exam_step_confs` (
  `id`                         BIGINT       NOT NULL AUTO_INCREMENT,
  `exam_conf_id`               BIGINT       NOT NULL,
  `sequence`                   INT(11)      NOT NULL,
  `name`                       VARCHAR(100) NOT NULL,
  `step_type`                  SMALLINT     NOT NULL,
  `mistakes_per_attempt_limit` INT(11)      NOT NULL,
  `attempts_limit`             INT(11)      NOT NULL,
  `has_to_be_submitted`        TINYINT(1)   NOT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (exam_conf_id)
  REFERENCES exam_confs (id)
    ON DELETE CASCADE
)
  DEFAULT CHARSET = utf8;

CREATE TABLE `exam_step_variant_confs` (
  `id`                BIGINT NOT NULL AUTO_INCREMENT,
  `exam_conf_id`      BIGINT NOT NULL,
  `exam_step_conf_id` BIGINT NOT NULL,
  `data_set_conf_id`  BIGINT NOT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (exam_step_conf_id)
  REFERENCES exam_step_confs (id)
    ON DELETE CASCADE
)
  DEFAULT CHARSET = utf8;

