ALTER TABLE `task_flow_step_confs`
  ADD COLUMN `calculation_precision` DECIMAL(8, 7) DEFAULT NULL,
  ADD COLUMN `is_result_info_step` TINYINT(1) NOT NULL
