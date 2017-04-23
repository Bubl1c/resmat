CREATE TABLE `problem_confs` (
  `id`                   BIGINT       NOT NULL                  AUTO_INCREMENT,
  `name`                 VARCHAR(100) NOT NULL,
  `problem_type`         SMALLINT     NOT NULL,
  `input_variable_confs` JSON         NOT NULL,
  PRIMARY KEY (`id`)
)
  DEFAULT CHARSET = utf8;

CREATE TABLE `problem_variant_confs` (
  `id`                    BIGINT NOT NULL AUTO_INCREMENT,
  `problem_conf_id`       BIGINT NOT NULL,
  `schema_url`            VARCHAR(255),
  `input_variable_values` JSON   NOT NULL,
  `calculated_data`       JSON   NOT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (problem_conf_id)
  REFERENCES problem_confs (id)
    ON DELETE CASCADE
)
  DEFAULT CHARSET = utf8;

