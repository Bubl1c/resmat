ALTER TABLE `test_group_confs`
  ADD COLUMN `parent_group_id` BIGINT DEFAULT NULL;
ALTER TABLE `test_group_confs`
  ADD FOREIGN KEY (parent_group_id) REFERENCES test_group_confs(id) ON UPDATE RESTRICT ON DELETE CASCADE;
