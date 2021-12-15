# Isoxya

Isoxya is an extensible data processing system for crawling and scraping the internet. It is written in a compiled, statically typed language for speed and reliability. Isoxya is free, open-source, and packaged as a container. The API can be used by CLI scripts or called by another project. Various plugins are available, many also open-source.

https://www.isoxya.com/  
https://hub.docker.com/r/isoxya/isoxya-api  
https://github.com/isoxya/isoxya-api  


## Dependencies

- [jq](https://stedolan.github.io/jq/) (for scripts only)


## Installation

Choose a stream: `stable` (recommended), `testing`, or `unstable`:

```sh
cd misc/streams/stable/
```

Boot the stack:

```sh
docker-compose up
```

That's it! Now you've got a powerful web crawler and scraper at your disposal, extensible via plugins.


## Isoxya Pro

Isoxya Pro adds high availability, error recovery, and horizontal scaling. It is available on-premises for installation on your servers, or in the cloud via SaaS subscription. Isoxya Pro is available via commercial licence. The API is able to scale crawlers, processors, and streamers. Support or custom development is available from Isoxya's creator.

https://www.isoxya.com/pro/  


## Contact

[tp@tiredpixel.com](mailto:tp@tiredpixel.com) · [www.tiredpixel.com](https://www.tiredpixel.com/) · [www.isoxya.com](https://www.isoxya.com/)

LinkedIn: [in/nic-williams](https://www.linkedin.com/in/nic-williams/) · Twitter: [tiredpixel](https://twitter.com/tiredpixel/) · GitHub: [tiredpixel](https://github.com/tiredpixel)


## Licence

Copyright © 2021 [Nic Williams](https://www.tiredpixel.com/). It is free software, released under the BSD 3-Clause licence, and may be redistributed under the terms specified in `LICENSE`.
