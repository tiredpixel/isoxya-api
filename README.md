# Isoxya

Isoxya is an extensible data processing system for crawling and scraping the internet. It is written in a compiled, statically typed language for speed and reliability. Isoxya is free, open-source, and packaged as a container. The API can be used by CLI scripts or called by another project. Various plugins are available, many also open-source.

https://www.isoxya.com/  
https://hub.docker.com/r/tiredpixel/isoxya-api  
https://github.com/tiredpixel/isoxya-api  


## Tutorial

To get started, follow the [tutorial](https://github.com/tiredpixel/isoxya-docs/blob/latest/Tutorial.md).


## Documentation

The API uses REST and JSON, and [documentation](https://github.com/tiredpixel/isoxya-docs) is available as a repo.


## Dependencies

- [jq](https://stedolan.github.io/jq/) (for `bin/` example scripts only)


## Installation

Compile and boot locally:

```sh
docker compose up
```

That's it! Now you've got a powerful web crawler and scraper at your disposal, extensible via plugins.

Images are also published using the `latest` tag (for development), and version-specific tags (for production). Do *not* use a `latest` tag in production!


## Isoxya Pro

Isoxya Pro adds high availability, error recovery, and horizontal scaling. It is available on-premises for installation on your servers, or in the cloud via SaaS subscription. Isoxya Pro is available via commercial licence. The API is able to scale crawlers, processors, and streamers. Support or custom development is available from Isoxya's creator.

https://www.isoxya.com/pro/  


## Licence

Copyright © [Nic Williams](https://www.tiredpixel.com/). It is free software, released under the BSD 3-Clause licence, and may be redistributed under the terms specified in `LICENSE`.
