CREATE TABLE `student_group_articles` (
  `student_group_id`      BIGINT        NOT NULL,
  `article_id`      BIGINT        NOT NULL,
  PRIMARY KEY (`student_group_id`, `article_id`),
  FOREIGN KEY (student_group_id)
  REFERENCES student_groups (id)
    ON DELETE CASCADE,
  FOREIGN KEY (article_id)
  REFERENCES articles (id)
    ON DELETE CASCADE
)
  DEFAULT CHARSET = utf8;