create or replace view gps_coord as
SELECT 
  id datetime, datetime::date as date, lokalitet, cowid, x(shape_utm) x,y(shape_utm) y
FROM 
  public.gpspoint;

-- View: movement_5s

-- DROP VIEW movement_5s;

CREATE OR REPLACE VIEW movement_5s AS 
 SELECT p1.cowid, p1.datetime, st_distance(p1.shape_utm, lead(p1.shape_utm) OVER (ORDER BY p1.datetime)) AS meters_obs, st_distance(p1.shape_utm, lead(p1.shape_utm, 1) OVER (ORDER BY p1.datetime)) AS meters_hour, observation.observation
   FROM gpspoint p1
   LEFT JOIN observation ON observation.gpspointid = p1.id, status
  WHERE p1.gpsstatus = status.id AND status.use = true;

ALTER TABLE movement_5s
  OWNER TO postgres;

alter view movement rename to movement_1h;