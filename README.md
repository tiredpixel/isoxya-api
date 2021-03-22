# Isoxya web crawler Community Edition (Isoxya CE)

[Isoxya web crawler Community Edition](https://github.com/isoxya/isoxya-ce) (Isoxya CE) is a free and open-source (BSD 3-Clause) mini crawler, suitable for small crawls on a single computer. It is part of [Isoxya](https://www.isoxya.com/) web crawler, an internet data processing system representing years of research into building next-generation crawlers and scrapers. 

Also available is [Isoxya web crawler Pro Edition](https://www.isoxya.com/) (Isoxya PE), a commercial and closed-source distributed crawler, suitable for small, large, and humongous crawls on high-availability clusters of multiple computers. Both editions utilise flexible [plugins](https://www.isoxya.com/plugins/), allowing numerous programming languages to be used to extend the core engine via JSON [interfaces](https://docs.isoxya.com/#interfaces). Plugins written for Isoxya CE should typically scale to Isoxya PE with minimal or no changes. More details and licences are available [on request](mailto:en@isoxya.com).


# Features

Feature             | Community Edition (CE) | Pro Edition (PE) |
--------------------|-------------|-------------|
Licence             | open-source | commercial  |
API                 | ✓           | ✓           |
CLI scripts         | ✓           | ✓           |
Plugins             | 3+          | 3+          |
·                   |             |             |
Authentication      | ✗           | Tigrosa     |
Database            | SQLite      | PostgreSQL  |
Cache               | ✗           | Redis       |
Message broker      | ✗           | RabbitMQ    |
·                   |             |             |
High-availability   | ✗           | ✓           |
Horizontal scaling  | ✗           | ✓           |
Error recovery      | ✗           | ✓           |
Resource management | ✗           | ✓           |
·                   |             |             |
Concurrent crawls   | 1           | ∞¹          |
Pages/crawl         | ∞²⁺³        | ∞¹          |
User-agents         | 1³          | ∞¹          |
Rate-limit (reqs/s) | 1/10³       | ∞¹⁺⁴        |
·                   |             |             |
Robots.txt          | ✗           | ✓           |
Crawl max pages     | ✗           | ✓           |
Crawl max depth     | ✗           | ✓           |
List crawls         | ✗           | ✓           |
External link check | ✗           | ✓           |
Crawl cancellation  | ✗           | ✓           |
Organisations       | ✗           | ✓           |
·                   |             |             |
Crawler channels    | 1           | ∞¹          |
Processor channels  | 1           | ∞¹          |
Streamer channels   | 1           | ∞¹          |
·                   |             |             |
OS variant          | Linux       | Linux       |
Packaging           | container   | container   |
Support             | community   | direct      |
·                   |             |             |
Price               | free        | [on request](mailto:en@isoxya.com) |

_Features and limits are indicative only, not guarantees._
_∞ indicates many, not infinite!_
_¹ depending on licence and infrastructure._
_² no hard-limit, but small as single-process._
_³ not configurable._
_⁴ set globally per-site; configurable for on-prem only._


## Usage

Choose an example stack, either `latest` or `stable` (recommended):

```sh
cd misc/eg/stable/
```

Initialise the stack:

```sh
./docker-compose.init.sh
```

Boot the stack. This includes: Isoxya CE API, using SQLite as an embedded database; [Isoxya plugin: Crawler HTML](https://github.com/isoxya/isoxya-plugin-crawler-html), a processor plugin for crawling static HTML; and [NGINX Test Upstream](https://github.com/tiredpixel/nginx-test-upstream), a simple echo server used to demonstrate a streamer plugin.

```sh
docker-compose up
```

That's installation completed! Now you've got a powerful mini crawler at your disposal, extendible via its flexible plugin system. Next you'll probably want to configure the stack; the easiest way to do that is to use [Tigrosa Scripts](https://github.com/tiredpixel/tigrosa-x-bin) and [Isoxya Scripts](https://github.com/isoxya/isoxya-x-bin). These are provided by way of example, but are also suitable for controlling the API via a CLI even in production. These scripts can be cloned somewhere, and included in your PATH when required.

```sh
cd $HOME
git clone git@github.com:tiredpixel/tigrosa-x-bin.git
git clone git@github.com:isoxya/isoxya-x-bin.git

PATH=$PATH:$HOME/isoxya-x-bin/bin::$HOME/tigrosa-x-bin/bin
```

Initialise a state directory for Tigrosa:

```sh
tgr-init
```

```txt
endpoint [http://localhost:8000]: 
usr.href [/usr/81848a8a-dc32-4f0b-87f3-02efd72836f5]: 
usr_key.href [/usr_key/0644fee4-848c-4d57-a742-37161431b3dc]: 
```

```json
{
  "t_now": "2021-03-19T13:36:32.270915096Z",
  "version": "0.0.0"
}
```

Register a processor plugin:

```sh
isx-create-plug-proc
```

```txt
Isoxya CE/PE

org.href (PE) []: 
url (CE/PE) [http://crawler-html.plugin.dev.isoxya.com:8000/data]: 
tag (CE/PE) [crawler-html]: 
pub (PE) [null]: 
chans (PE) [null]: 
```

```json
{
  "href": "/plug_proc/b72b6972-210b-4795-9b36-9a634e2c4ce9",
  "tag": "crawler-html",
  "url": "http://crawler-html.plugin.dev.isoxya.com:8000/data"
}
```

Register a streamer plugin:

```sh
isx-create-plug-strm
```

```txt
Isoxya CE/PE

org.href (PE) []: 
url (CE/PE) [http://elasticsearch.plugin.dev.isoxya.com:8000/data]: http://test_upstream
tag (CE/PE) [elasticsearch]: test-upstream
pub (PE) [null]: 
chans (PE) [null]: 
```

```json
{
  "href": "/plug_strm/0a2dfa6d-d4b1-45e0-8167-9fd78e128c6c",
  "tag": "test-upstream",
  "url": "http://test_upstream"
}
```

Register a site:

```sh
isx-create-site
```

```txt
Isoxya CE/PE

url (CE/PE) [http://example.com]: 
rate_lim (PE) [null]: 
chans (PE) [null]: 
```

```json
{
  "href": "/site/aHR0cDovL2V4YW1wbGUuY29tOjgw",
  "url": "http://example.com:80"
}
```

Start a crawl:

```sh
isx-create-crwl
```

```txt
Isoxya CE/PE

site.href (CE/PE) [/site/aHR0cDovL2V4YW1wbGUuY29tOjgw]: 
pages_max (PE) [null]: 
depth_max (PE) [null]: 
user_agent.href (PE) []: 
list.href (PE):
    0: null
    1: 
  [0]: 
  null
validate_ext (PE) [null]: 
org.href (PE) []: 
plug_proc_conf (CE/PE) [null]: 
plug_proc.hrefs (CE/PE) [/plug_proc/b72b6972-210b-4795-9b36-9a634e2c4ce9]: 
plug_strm.hrefs (CE/PE) [/plug_strm/0a2dfa6d-d4b1-45e0-8167-9fd78e128c6c]: 
```

```json
{
  "href": "/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/crwl/2021-03-19T13:43:07.733671786Z",
  "pages": null,
  "plug_proc": [
    {
      "href": "/plug_proc/b72b6972-210b-4795-9b36-9a634e2c4ce9"
    }
  ],
  "plug_proc_conf": null,
  "plug_strm": [
    {
      "href": "/plug_strm/0a2dfa6d-d4b1-45e0-8167-9fd78e128c6c"
    }
  ],
  "progress": null,
  "site": {
    "href": "/site/aHR0cDovL2V4YW1wbGUuY29tOjgw",
    "url": "http://example.com:80"
  },
  "status": "pending",
  "t_begin": "2021-03-19T13:43:07.733671786Z",
  "t_end": null
}
```

Read resources to check status:

```sh
isx-read
```

```txt
Isoxya CE/PE

href (CE/PE):
    0: 
    1: /plug_proc/b72b6972-210b-4795-9b36-9a634e2c4ce9
    2: /plug_strm/0a2dfa6d-d4b1-45e0-8167-9fd78e128c6c
    3: 
    4: /site/aHR0cDovL2V4YW1wbGUuY29tOjgw
    5: 
    6: /site/aHR0cDovL2V4YW1wbGUuY29tOjgw/crwl/2021-03-19T13:43:07.733671786Z
  [6]: 
  /site/aHR0cDovL2V4YW1wbGUuY29tOjgw/crwl/2021-03-19T13:43:07.733671786Z
```

```json
{
  "href": "/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/crwl/2021-03-19T13:43:07.733671786Z",
  "pages": 1,
  "plug_proc": [
    {
      "href": "/plug_proc/b72b6972-210b-4795-9b36-9a634e2c4ce9"
    }
  ],
  "plug_proc_conf": null,
  "plug_strm": [
    {
      "href": "/plug_strm/0a2dfa6d-d4b1-45e0-8167-9fd78e128c6c"
    }
  ],
  "progress": 100,
  "site": {
    "href": "/site/aHR0cDovL2V4YW1wbGUuY29tOjgw",
    "url": "http://example.com:80"
  },
  "status": "completed",
  "t_begin": "2021-03-19T13:43:07.733671786Z",
  "t_end": "2021-03-19T13:43:08.801Z"
}
```

To crawl again, just use `isx-create-crwl`. To crawl another site, just register it with `isx-create-site` first. That's it!

When you're ready to do something more complicated, you might like to take a look at other [Isoxya plugins](https://www.isoxya.com/plugins/). And of course, you can create your own plugins in your language of choice, whether processor plugins to extract page data differently, or streamer plugins to send data to your API endpoint or databases. If you create something cool, be sure to let me know!


## Contact

[en@isoxya.com](mailto:en@isoxya.com) · [isoxya.com](https://www.isoxya.com/)

[tp@tiredpixel.com](mailto:tp@tiredpixel.com) · [tiredpixel.com](https://www.tiredpixel.com/)

LinkedIn: [in/nic-williams](https://www.linkedin.com/in/nic-williams/) · GitHub: [tiredpixel](https://github.com/tiredpixel)


## Licence

Copyright © 2021 [Nic Williams](https://www.tiredpixel.com/). It is free software, released under the BSD 3-Clause licence, and may be redistributed under the terms specified in `LICENSE`.
