ALTER TABLE metric ALTER COLUMN options SET DEFAULT '{}';
UPDATE metric SET options = '{}' WHERE options IS NULL;
ALTER TABLE metric ALTER COLUMN options SET NOT NULL;