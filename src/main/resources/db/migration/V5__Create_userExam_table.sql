CREATE TABLE `user_exams` (
  `id`                   BIGINT   NOT NULL                  AUTO_INCREMENT,
  `user_id`              BIGINT   NOT NULL,
  `exam_conf_id`         BIGINT   NOT NULL,
  `current_step_conf_id` BIGINT   NOT NULL,
  `status`               SMALLINT NOT NULL,
  `started`              DATETIME NULL                      DEFAULT NULL,
  `finished`             DATETIME NULL                      DEFAULT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (user_id)
  REFERENCES users (id)
    ON DELETE CASCADE,
  FOREIGN KEY (exam_conf_id)
  REFERENCES exam_confs (id)
    ON DELETE CASCADE,
  FOREIGN KEY (current_step_conf_id)
  REFERENCES exam_step_confs (id)
    ON DELETE CASCADE
)
  DEFAULT CHARSET = utf8;

CREATE TABLE `user_exam_step_attempts` (
  `id`                BIGINT   NOT NULL                  AUTO_INCREMENT,
  `user_exam_id`      BIGINT   NOT NULL,
  `exam_step_conf_id` BIGINT   NOT NULL,
  `mistakes_amount`   INT(11)  NOT NULL,
  `attempt_number`    INT(11)  NOT NULL,
  `status`            SMALLINT NOT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (user_exam_id)
  REFERENCES user_exams (id)
    ON DELETE CASCADE,
  FOREIGN KEY (exam_step_conf_id)
  REFERENCES exam_step_confs (id)
    ON DELETE CASCADE
)
  DEFAULT CHARSET = utf8;

CREATE TABLE `user_exam_results` (
  `id`                 BIGINT       NOT NULL                  AUTO_INCREMENT,
  `user_exam_id`       BIGINT       NOT NULL,
  `exam_conf_id`       BIGINT       NOT NULL,
  `user_id`            BIGINT       NOT NULL,
  `exam_name`          VARCHAR(100) NOT NULL,
  `student_name`       VARCHAR(255) NOT NULL,
  `student_group_name` VARCHAR(100) NOT NULL,
  `duration_millis`    BIGINT       NOT NULL,
  `step_results`       JSON         NOT NULL,
  `score`              INT(11)      NOT NULL,
  `max_score`          INT(11)      NOT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (user_id)
  REFERENCES users (id)
    ON DELETE CASCADE
)
  DEFAULT CHARSET = utf8;

