# ⚾ Pelotero Engine ⚾

[![Haskell CI](https://github.com/cardanonix/pelotero-engine/actions/workflows/haskell.yml/badge.svg?branch=main&event=status)](https://github.com/cardanonix/pelotero-engine/actions/workflows/haskell.yml)

This app provides functions to scrape game data from a baseball API for a given date range and process the retrieved data.

Ensure that you have the necessary Haskell libraries installed, such as:

- HTTP.Simple
- Time
- ByteString
- Vector
- Map
- Text
- Aeson
- Generics
- Crypto
- Directory
- Monad
- Async

If you use nix, making sure all dependencies are covered is as simple as cloning and running:
```
nix develop github:cardanonix/pelotero-engine
```


# Features
- Scraping Game Data: Using baseball API endpoints, this module retrieves game schedules, live game statuses, and box scores.

- Async Fetch: Instead of fetching data for each game one by one, this module offers an asynchronous version that can fetch data for multiple games concurrently.

- Output Conversion: This scraper not only fetches data but also processes it and converts it into a user-friendly JSON format suitable for downstream consumption.

- Utility Functions: Various utility functions to handle date manipulation, filename formatting, data fetching and decoding, and more.

- Error Handling: Effective error handling mechanisms to ensure smooth operation.

- Integration with ADTs: The module leverages Abstract Data Types (Input and Middle) for structured data processing.

# Functions
- fetchGameScheduleForDate: Fetches the game schedule for a given date.

- fetchGameStatus: Fetches the live status of a game given its game ID.

- fetchFinishedBxScore: Checks if a game has finished and fetches its box score if it has.

- fetchFinishedBxScores: Async version that fetches box scores for multiple games concurrently.

- fetchFinishedBxScoresToJsonPlayerData: Converts the fetched game data to a JSON player data format.

- processDate: Processes a given date by fetching game schedule and game data, then outputs them.

- scrapeDataForDateRange: Main function that processes data scraping for a given date range.


# Usage
To scrape data for a specific date range, you can use the scrapeDataForDateRange function, providing the start and end date:

```
nix develop github:harryprayiv/scraperProto
cabal run fetchStats 2023-08-22 2023-08-22
```

# Note
This app focuses on a baseball API and its structure at the time of the last update. It's advisable to ensure that the API's structure or endpoints haven't changed significantly before using this scraper for production purposes.

# Contribute
Feel free to fork, improve, create pull requests, report bugs, or request new features.

Made with ❤️ by [Your Name].
