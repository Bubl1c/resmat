ALTER TABLE `user_exam_step_attempt_test_set_tests`
  ADD COLUMN `mistake_value` FLOAT DEFAULT NULL;

ALTER TABLE `test_set_conf_test_group_confs`
  ADD COLUMN `mistake_value` FLOAT DEFAULT NULL;
