# Isoxya

Isoxya is an extensible data processing system for crawling and scraping the internet. It is written in a compiled, statically typed language for speed and reliability. Isoxya is free, open-source, and packaged as a container. The API can be used by CLI scripts or called by another project. Various plugins are available, many also open-source.

https://www.isoxya.com/  
https://hub.docker.com/r/isoxya/isoxya-api  
https://github.com/isoxya/isoxya-api  


## Dependencies

- [jq](https://stedolan.github.io/jq/) (for scripts only)


## Installation

Choose an example stack, either `stable` (recommended) or `latest`:

```sh
cd misc/eg/stable/
```

Initialise the stack:

```sh
./docker-compose.init.sh
```

Boot the stack:

```sh
docker-compose up
```

That's it! Now you've got a powerful web crawler and scraper at your disposal, extensible via plugins.


## Tutorial

Initialise a state directory:

```sh
isoxya-api-init
```

```txt
endpoint [http://localhost:8000]: 
```

```json
{
  "now": "2021-12-02T11:49:43.207658439Z",
  "version": "0.0.0"
}
```

Register a processor plugin:

```sh
isoxya-api-create-processor
```

```txt
url [http://isoxya-plugin-crawler-html.localhost:8000/data]: 
tag [crawler-html]: 
```

```json
{
  "href": "/processor/2a46ecef-0db8-47d2-9f0a-de84429ea010",
  "tag": "crawler-html",
  "url": "http://isoxya-plugin-crawler-html.localhost:8000/data"
}
```

Register a streamer plugin:

```sh
isoxya-api-create-streamer
```

```txt
url [http://isoxya-plugin-elasticsearch.localhost:8000/data]: 
tag [elasticsearch]: 
```

```json
{
  "href": "/streamer/1b56af7a-82a6-4d4f-b71c-244f326b5319",
  "tag": "elasticsearch",
  "url": "http://isoxya-plugin-elasticsearch.localhost:8000/data"
}
```

Register a site:

```sh
isoxya-api-create-site
```

```txt
url [http://example.com]: 
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


## Isoxya Pro

Isoxya Pro adds high availability, error recovery, and horizontal scaling. It is available on-premises for installation on your servers, or in the cloud via SaaS subscription. Isoxya Pro is available via commercial licence. The API is able to scale crawlers, processors, and streamers. Support or custom development is available from Isoxya's creator.

https://www.isoxya.com/pro/  


## Contact

[tp@tiredpixel.com](mailto:tp@tiredpixel.com) · [www.tiredpixel.com](https://www.tiredpixel.com/) · [www.isoxya.com](https://www.isoxya.com/)

LinkedIn: [in/nic-williams](https://www.linkedin.com/in/nic-williams/) · Twitter: [tiredpixel](https://twitter.com/tiredpixel/) · GitHub: [tiredpixel](https://github.com/tiredpixel)


## Licence

Copyright © 2021 [Nic Williams](https://www.tiredpixel.com/). It is free software, released under the BSD 3-Clause licence, and may be redistributed under the terms specified in `LICENSE`.
