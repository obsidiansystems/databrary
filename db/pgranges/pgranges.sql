CREATE OR REPLACE FUNCTION segments_union(SEGMENT [], SEGMENT [])
  RETURNS SEGMENT [] IMMUTABLE STRICT LANGUAGE C AS 'pgranges.so', 'ranges_union';
CREATE OR REPLACE FUNCTION segments_union(SEGMENT [], SEGMENT)
  RETURNS SEGMENT [] IMMUTABLE STRICT LANGUAGE C AS 'pgranges.so', 'ranges_union1';
CREATE AGGREGATE "segments_union" ( SEGMENT ) ( SFUNC = segments_union, STYPE = SEGMENT [], INITCOND = '{}');
