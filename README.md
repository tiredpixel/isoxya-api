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
  "now": "2021-12-02T13:35:22.149003003Z",
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
  "href": "/processor/17f17cef-6eb3-4bf4-bbf0-a3d729b3d650",
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
  "href": "/streamer/68e526ee-dd89-4a0e-932f-bf23825fabd0",
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
isoxya-api-create-crawl
```

```txt
site.href [/site/aHR0cDovL2V4YW1wbGUuY29tOjgw]: 
processor_config [null]: 
processors.hrefs [/processor/17f17cef-6eb3-4bf4-bbf0-a3d729b3d650]: 
streamers.hrefs [/streamer/68e526ee-dd89-4a0e-932f-bf23825fabd0]: 
```

```json
{
  "began": "2021-12-02T13:37:02.063613916Z",
  "ended": null,
  "href": "/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/crawl/2021-12-02T13:37:02.063613916Z",
  "pages": null,
  "processor_config": null,
  "processors": [
    {
      "href": "/processor/17f17cef-6eb3-4bf4-bbf0-a3d729b3d650"
    }
  ],
  "progress": null,
  "site": {
    "href": "/site/aHR0cDovL2V4YW1wbGUuY29tOjgw",
    "url": "http://example.com:80"
  },
  "status": "pending",
  "streamers": [
    {
      "href": "/streamer/68e526ee-dd89-4a0e-932f-bf23825fabd0"
    }
  ]
}
```

Read resources to check status:

```sh
isoxya-api-read
```

```txt
href:
    0: /processor/17f17cef-6eb3-4bf4-bbf0-a3d729b3d650
    1: /streamer/68e526ee-dd89-4a0e-932f-bf23825fabd0
    2: 
    3: /site/aHR0cDovL2V4YW1wbGUuY29tOjgw
    4: 
    5: /site/aHR0cDovL2V4YW1wbGUuY29tOjgw/crawl/2021-12-02T13:37:02.063613916Z
  [5]: 
  /site/aHR0cDovL2V4YW1wbGUuY29tOjgw/crawl/2021-12-02T13:37:02.063613916Z
```

```json
{
  "began": "2021-12-02T13:37:02.063613916Z",
  "ended": "2021-12-02T13:37:02.596Z",
  "href": "/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/crawl/2021-12-02T13:37:02.063613916Z",
  "pages": 1,
  "processor_config": null,
  "processors": [
    {
      "href": "/processor/17f17cef-6eb3-4bf4-bbf0-a3d729b3d650"
    }
  ],
  "progress": 100,
  "site": {
    "href": "/site/aHR0cDovL2V4YW1wbGUuY29tOjgw",
    "url": "http://example.com:80"
  },
  "status": "completed",
  "streamers": [
    {
      "href": "/streamer/68e526ee-dd89-4a0e-932f-bf23825fabd0"
    }
  ]
}
```

To crawl again, just use `isoxya-api-create-crawl`. To crawl another site, just register it with `isoxya-api-create-site` first. That's it!


## Isoxya Pro

Isoxya Pro adds high availability, error recovery, and horizontal scaling. It is available on-premises for installation on your servers, or in the cloud via SaaS subscription. Isoxya Pro is available via commercial licence. The API is able to scale crawlers, processors, and streamers. Support or custom development is available from Isoxya's creator.

https://www.isoxya.com/pro/  


## Contact

[tp@tiredpixel.com](mailto:tp@tiredpixel.com) · [www.tiredpixel.com](https://www.tiredpixel.com/) · [www.isoxya.com](https://www.isoxya.com/)

LinkedIn: [in/nic-williams](https://www.linkedin.com/in/nic-williams/) · Twitter: [tiredpixel](https://twitter.com/tiredpixel/) · GitHub: [tiredpixel](https://github.com/tiredpixel)


## Licence

Copyright © 2021 [Nic Williams](https://www.tiredpixel.com/). It is free software, released under the BSD 3-Clause licence, and may be redistributed under the terms specified in `LICENSE`.
