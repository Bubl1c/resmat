CREATE TABLE `task_flow_confs` (
  `id`              BIGINT       NOT NULL                  AUTO_INCREMENT,
  `problem_conf_id` BIGINT       NOT NULL,
  `name`            VARCHAR(500) NOT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (problem_conf_id)
  REFERENCES problem_confs (id)
    ON DELETE CASCADE
)
  DEFAULT CHARSET = utf8;

CREATE TABLE `task_flow_step_confs` (
  `id`                BIGINT       NOT NULL                  AUTO_INCREMENT,
  `task_flow_conf_id` BIGINT       NOT NULL,
  `name`              VARCHAR(500) NOT NULL,
  `sequence`          SMALLINT     NOT NULL,
  `step_type`         SMALLINT     NOT NULL,
  `step_data`         JSON         NOT NULL,
  `is_help_step`      TINYINT(1)   NOT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (task_flow_conf_id)
  REFERENCES task_flow_confs (id)
    ON DELETE CASCADE
)
  DEFAULT CHARSET = utf8;

CREATE TABLE `user_exam_task_flows` (
  `id`                      BIGINT   NOT NULL                  AUTO_INCREMENT,
  `step_attempt_id`         BIGINT   NOT NULL,
  `task_flow_conf_id`       BIGINT   NOT NULL,
  `problem_variant_conf_id` BIGINT   NOT NULL,
  `current_step_sequence`   SMALLINT NOT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (step_attempt_id)
  REFERENCES user_exam_step_attempts (id)
    ON DELETE CASCADE,
  FOREIGN KEY (task_flow_conf_id)
  REFERENCES task_flow_confs (id)
    ON DELETE RESTRICT ,
  FOREIGN KEY (problem_variant_conf_id)
  REFERENCES problem_variant_confs (id)
    ON DELETE RESTRICT
)
  DEFAULT CHARSET = utf8;

CREATE TABLE `user_exam_task_flow_steps` (
  `id`                        BIGINT     NOT NULL                  AUTO_INCREMENT,
  `step_attempt_task_flow_id` BIGINT     NOT NULL,
  `task_flow_step_conf_id`    BIGINT     NOT NULL,
  `sequence`                  SMALLINT   NOT NULL,
  `answer`                    JSON       NOT NULL,
  `done`                      TINYINT(1) NOT NULL,
  `mistakes`                  SMALLINT   NOT NULL                  DEFAULT 0,
  PRIMARY KEY (`id`),
  FOREIGN KEY (step_attempt_task_flow_id)
  REFERENCES user_exam_task_flows (id)
    ON DELETE CASCADE,
  FOREIGN KEY (task_flow_step_conf_id)
  REFERENCES task_flow_step_confs (id)
    ON DELETE RESTRICT
)
  DEFAULT CHARSET = utf8;

