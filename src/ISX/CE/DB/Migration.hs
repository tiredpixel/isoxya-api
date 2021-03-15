module ISX.CE.DB.Migration (migrations) where


import           TPX.Com.SQLite.Query
import qualified TPX.Com.SQLite.Conn  as D
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--  Generate id via
--      date +%s
migrations :: [(Integer, D.Conn -> IO ())]
migrations = [
    (1614774314, m1614774314)]
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
m1614774314 :: D.Conn -> IO ()
m1614774314 d =
    executeW' q9 d >>
    executeW' q13 d >>
    executeW' q14 d >>
    executeW' q18 d >>
    executeW' q24 d >>
    executeW' q27 d >>
    executeW' q28 d >>
    executeW' q31 d >>
    executeW' q32 d >>
    executeW' q33 d >>
    executeW' q52 d >>
    executeW' q56 d >>
    executeW' q61 d >>
    executeW' q66 d >>
    executeW' q67 d >>
    executeW' q72 d >>
    executeW' q73 d
    where
        q9 = " \
        \   /* m1614774314.9 */ \
        \   CREATE TABLE plug_proc ( \
        \       plug_proc_id BLOB NOT NULL PRIMARY KEY, \
        \       url          TEXT NOT NULL UNIQUE, \
        \       tag          TEXT NOT NULL, \
        \       t_ins        TEXT NOT NULL DEFAULT(STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW')) \
        \   ); \
        \ "
        q13 = " \
        \   /* m1614774314.13 */ \
        \   CREATE VIEW d_plug_proc_0 AS \
        \    SELECT plug_proc.plug_proc_id, \
        \       plug_proc.url, \
        \       plug_proc.tag, \
        \       plug_proc.t_ins \
        \      FROM plug_proc; \
        \ "
        q14 = " \
        \   /* m1614774314.14 */ \
        \   CREATE TABLE plug_strm ( \
        \       plug_strm_id BLOB NOT NULL PRIMARY KEY, \
        \       url          TEXT NOT NULL UNIQUE, \
        \       tag          TEXT NOT NULL, \
        \       t_ins        TEXT NOT NULL DEFAULT(STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW')) \
        \   ); \
        \ "
        q18 = " \
        \   /* m1614774314.18 */ \
        \   CREATE VIEW d_plug_strm_0 AS \
        \    SELECT plug_strm.plug_strm_id, \
        \       plug_strm.url, \
        \       plug_strm.tag, \
        \       plug_strm.t_ins \
        \      FROM plug_strm; \
        \ "
        q24 = " \
        \   /* m1614774314.24 */ \
        \   CREATE TABLE site ( \
        \       site_id BLOB    NOT NULL PRIMARY KEY, \
        \       url     TEXT    NOT NULL UNIQUE, \
        \       auto    INTEGER NOT NULL DEFAULT false, \
        \       t_ins   TEXT    NOT NULL DEFAULT(STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW')) \
        \   ); \
        \ "
        q27 = " \
        \   /* m1614774314.27 */ \
        \   CREATE VIEW d_site_0 AS \
        \    SELECT site.site_id, \
        \       site.url, \
        \       site.t_ins \
        \      FROM site; \
        \ "
        q28 = " \
        \   /* m1614774314.28 */ \
        \   CREATE TABLE page ( \
        \       page_id BLOB NOT NULL PRIMARY KEY, \
        \       site_id BLOB NOT NULL REFERENCES site (site_id) ON DELETE CASCADE, \
        \       url     TEXT NOT NULL \
        \   ); \
        \ "
        q31 = " \
        \   /* m1614774314.31 */ \
        \   CREATE INDEX page_url_idx ON page (site_id, url); \
        \ "
        q32 = " \
        \   /* m1614774314.32 */ \
        \   CREATE VIEW d_page_0 AS \
        \    SELECT page.page_id, \
        \       page.site_id, \
        \       page.url \
        \      FROM page; \
        \ "
        q33 = " \
        \   /* m1614774314.33 */ \
        \   CREATE TABLE crwl ( \
        \       site_id        BLOB NOT NULL REFERENCES site (site_id), \
        \       site_v         TEXT NOT NULL DEFAULT(STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW')), \
        \       status         TEXT NOT NULL DEFAULT 'Pending', \
        \       plug_proc_conf TEXT NOT NULL DEFAULT 'null', \
        \       PRIMARY KEY (site_id, site_v) \
        \   ); \
        \ "
        q52 = " \
        \   /* m1614774314.52 */ \
        \   CREATE TABLE crwl_plug_proc ( \
        \       site_id      BLOB NOT NULL, \
        \       site_v       TEXT NOT NULL, \
        \       plug_proc_id BLOB NOT NULL REFERENCES plug_proc (plug_proc_id), \
        \       PRIMARY KEY (site_id, site_v, plug_proc_id), \
        \       FOREIGN KEY (site_id, site_v) REFERENCES crwl (site_id, site_v) ON DELETE CASCADE \
        \   ); \
        \ "
        q56 = " \
        \   /* m1614774314.56 */ \
        \   CREATE TABLE crwl_plug_strm ( \
        \       site_id      BLOB NOT NULL, \
        \       site_v       TEXT NOT NULL, \
        \       plug_strm_id BLOB NOT NULL REFERENCES plug_strm (plug_strm_id), \
        \       PRIMARY KEY (site_id, site_v, plug_strm_id), \
        \       FOREIGN KEY (site_id, site_v) REFERENCES crwl (site_id, site_v) ON DELETE CASCADE \
        \   ); \
        \ "
        q61 = " \
        \   /* m1614774314.61 */ \
        \   CREATE TABLE crwl_page ( \
        \       site_id        BLOB NOT NULL, \
        \       site_v         TEXT NOT NULL, \
        \       page_id        BLOB NOT NULL, \
        \       page_v         TEXT, \
        \       plug_proc_id   BLOB NOT NULL REFERENCES plug_proc (plug_proc_id), \
        \       p_page_id      BLOB, \
        \       p_page_v       TEXT, \
        \       p_plug_proc_id BLOB          REFERENCES plug_proc (plug_proc_id), \
        \       t_ins          TEXT, \
        \       PRIMARY KEY (site_id, site_v, page_id, plug_proc_id), \
        \       FOREIGN KEY (site_id, site_v) REFERENCES crwl (site_id, site_v) ON DELETE CASCADE \
        \   ); \
        \ "
        q66 = " \
        \   /* m1614774314.66 */ \
        \   CREATE INDEX crwl_page_ins_idx ON crwl_page (t_ins, site_id, site_v); \
        \ "
        q67 = " \
        \   /* m1614774314.67 */ \
        \   CREATE INDEX crwl_page_p_idx ON crwl_page (site_id, site_v, p_page_id, p_page_v); \
        \ "
        q72 = " \
        \   /* m1614774314.72 */ \
        \   CREATE VIEW stat_crwl_prog AS \
        \    SELECT crwl_page.site_id, \
        \       crwl_page.site_v, \
        \       crwl_page.plug_proc_id, \
        \       count(crwl_page.page_id) AS pages, \
        \       count(crwl_page.page_v) AS processed, \
        \       cast(((count(crwl_page.page_v) * 100.0) / count(crwl_page.page_id)) AS INT) AS progress \
        \      FROM crwl_page \
        \     GROUP BY crwl_page.site_id, crwl_page.site_v, crwl_page.plug_proc_id; \
        \ "
        q73 = " \
        \   /* m1614774314.73 */ \
        \   CREATE VIEW d_crwl_0 AS \
        \   SELECT \
        \       c.site_id, \
        \       c.site_v, \
        \       (CASE (c.status = 'Pending' AND cast(((sum(x.progress) * 1.0) / count(x.site_id)) AS INT) = 100) \
        \           WHEN true THEN 'Completed' \
        \           ELSE c.status \
        \          END \
        \       ) AS status, \
        \       cast(round((sum(x.pages) * 1.0) / count(x.site_id)) AS INT) AS pages, \
        \       cast(((sum(x.processed) * 1.0) / count(x.site_id)) AS INT) AS processed, \
        \       cast(((sum(x.progress) * 1.0) / count(x.site_id)) AS INT) AS progress, \
        \       ( SELECT max(cd.t_ins) AS max \
        \              FROM crwl_page cd \
        \             WHERE (c.site_id, c.site_v) = (cd.site_id, cd.site_v)) AS t_end, \
        \       c.plug_proc_conf, \
        \       (SELECT cast(group_concat(cpp.plug_proc_id) AS BLOB) \
        \           FROM crwl_plug_proc cpp \
        \           WHERE (c.site_id, c.site_v) = (cpp.site_id, cpp.site_v) \
        \       ) AS plug_proc_ids, \
        \       (SELECT cast(group_concat(cps.plug_strm_id) AS BLOB) \
        \           FROM crwl_plug_strm cps \
        \           WHERE (c.site_id, c.site_v) = (cps.site_id, cps.site_v) \
        \       ) AS plug_strm_ids \
        \   FROM \
        \       crwl c \
        \       LEFT JOIN \
        \           stat_crwl_prog x \
        \           ON (c.site_id, c.site_v) = (x.site_id, x.site_v) \
        \   GROUP BY \
        \       c.site_id, \
        \       c.site_v \
        \ "
--------------------------------------------------------------------------------
