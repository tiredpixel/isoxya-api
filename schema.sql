CREATE TABLE _meta (        k TEXT PRIMARY KEY,        v TEXT NOT NULL    );
CREATE TABLE processor (        processor_id BLOB NOT NULL PRIMARY KEY,        url          TEXT NOT NULL UNIQUE,        tag          TEXT NOT NULL,        inserted     TEXT NOT NULL DEFAULT(STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW'))    );
CREATE VIEW processor_0 AS     SELECT processor.processor_id,        processor.url,        processor.tag,        processor.inserted       FROM processor
/* processor_0(processor_id,url,tag,inserted) */;
CREATE TABLE streamer (        streamer_id BLOB NOT NULL PRIMARY KEY,        url         TEXT NOT NULL UNIQUE,        tag         TEXT NOT NULL,        inserted    TEXT NOT NULL DEFAULT(STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW'))    );
CREATE VIEW streamer_0 AS     SELECT streamer.streamer_id,        streamer.url,        streamer.tag,        streamer.inserted       FROM streamer
/* streamer_0(streamer_id,url,tag,inserted) */;
CREATE TABLE site (        site_id  BLOB    NOT NULL PRIMARY KEY,        url      TEXT    NOT NULL UNIQUE,        auto     INTEGER NOT NULL DEFAULT false,        inserted TEXT    NOT NULL DEFAULT(STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW'))    );
CREATE VIEW site_0 AS     SELECT site.site_id,        site.url,        site.inserted       FROM site
/* site_0(site_id,url,inserted) */;
CREATE TABLE page (        page_id BLOB NOT NULL PRIMARY KEY,        site_id BLOB NOT NULL REFERENCES site (site_id) ON DELETE CASCADE,        url     TEXT NOT NULL    );
CREATE INDEX page_url_idx ON page (site_id, url);
CREATE VIEW page_0 AS     SELECT page.page_id,        page.site_id,        page.url       FROM page
/* page_0(page_id,site_id,url) */;
CREATE TABLE crawl (        site_id          BLOB NOT NULL REFERENCES site (site_id),        site_v           TEXT NOT NULL DEFAULT(STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW')),        status           TEXT NOT NULL DEFAULT 'Pending',        processor_config TEXT NOT NULL DEFAULT 'null',        PRIMARY KEY (site_id, site_v)    );
CREATE TABLE crawl_processor (        site_id      BLOB NOT NULL,        site_v       TEXT NOT NULL,        processor_id BLOB NOT NULL REFERENCES processor (processor_id),        PRIMARY KEY (site_id, site_v, processor_id),        FOREIGN KEY (site_id, site_v) REFERENCES crawl (site_id, site_v) ON DELETE CASCADE    );
CREATE TABLE crawl_streamer (        site_id     BLOB NOT NULL,        site_v      TEXT NOT NULL,        streamer_id BLOB NOT NULL REFERENCES streamer (streamer_id),        PRIMARY KEY (site_id, site_v, streamer_id),        FOREIGN KEY (site_id, site_v) REFERENCES crawl (site_id, site_v) ON DELETE CASCADE    );
CREATE TABLE crawl_page (        site_id             BLOB NOT NULL,        site_v              TEXT NOT NULL,        page_id             BLOB NOT NULL,        page_v              TEXT,        processor_id        BLOB NOT NULL REFERENCES processor (processor_id),        parent_page_id      BLOB,        parent_page_v       TEXT,        parent_processor_id BLOB          REFERENCES processor (processor_id),        inserted            TEXT,        PRIMARY KEY (site_id, site_v, page_id, processor_id),        FOREIGN KEY (site_id, site_v) REFERENCES crawl (site_id, site_v) ON DELETE CASCADE    );
CREATE INDEX crawl_page_ins_idx ON crawl_page (inserted, site_id, site_v);
CREATE INDEX crawl_page_parent_idx ON crawl_page (site_id, site_v, parent_page_id, parent_page_v);
CREATE VIEW stat_crawl_prog AS     SELECT crawl_page.site_id,        crawl_page.site_v,        crawl_page.processor_id,        count(crawl_page.page_id) AS page,        count(crawl_page.page_v) AS processed,        cast(((count(crawl_page.page_v) * 100.0) / count(crawl_page.page_id)) AS INT) AS progress       FROM crawl_page      GROUP BY crawl_page.site_id, crawl_page.site_v, crawl_page.processor_id
/* stat_crawl_prog(site_id,site_v,processor_id,page,processed,progress) */;
CREATE VIEW crawl_0 AS    SELECT        c.site_id,        c.site_v,        (CASE (c.status = 'Pending' AND cast(((sum(x.progress) * 1.0) / count(x.site_id)) AS INT) = 100)            WHEN true THEN 'Completed'            ELSE c.status           END        ) AS status,        cast(round((sum(x.page) * 1.0) / count(x.site_id)) AS INT) AS page,        cast(((sum(x.processed) * 1.0) / count(x.site_id)) AS INT) AS processed,        cast(((sum(x.progress) * 1.0) / count(x.site_id)) AS INT) AS progress,        ( SELECT max(cd.inserted) AS max               FROM crawl_page cd              WHERE (c.site_id, c.site_v) = (cd.site_id, cd.site_v)) AS inserted,        c.processor_config,        (SELECT cast(group_concat(cpp.processor_id) AS BLOB)            FROM crawl_processor cpp            WHERE (c.site_id, c.site_v) = (cpp.site_id, cpp.site_v)        ) AS processor_ids,        (SELECT cast(group_concat(cps.streamer_id) AS BLOB)            FROM crawl_streamer cps            WHERE (c.site_id, c.site_v) = (cps.site_id, cps.site_v)        ) AS streamer_ids    FROM        crawl c        LEFT JOIN            stat_crawl_prog x            ON (c.site_id, c.site_v) = (x.site_id, x.site_v)    GROUP BY        c.site_id,        c.site_v
/* crawl_0(site_id,site_v,status,page,processed,progress,inserted,processor_config,processor_ids,streamer_ids) */;
