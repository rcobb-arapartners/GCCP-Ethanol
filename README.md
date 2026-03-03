# Crush Spreads

A Shiny app for calculating and visualizing ethanol crush spreads across ON and OFF corn contract months, powered by daily price data from the MarketView API.

---

## What It Does

The app produces two tables of crush spreads — one for ON corn months and one for OFF corn months — organized by contract year and month. Each table is paired with a historical time series chart. Expired contract months are valued using the full-month average of daily closes. Active forward months use the latest implied spread from the current closing price.

---

## Files

| File | Description |
|---|---|
| `app.R` | Main Shiny application (UI + server) |
| `01-MetaLoad.R` | Package loading and theme definitions |
| `02-DataBuild.R` | MarketView API fetch and crush spread calculation |
| `03-VisualBuild.R` | gt table and plotly chart builders |
| `current-day-crush-data/` | Auto-created cache folder (not committed to git) |

---

## Setup

### Requirements

```r
install.packages(c(
  "shiny", "bslib", "shinyjs", "tidyverse",
  "lubridate", "gt", "plotly"
))
```

### Environment Variables

The app requires MarketView API credentials set as environment variables:

```bash
ARA_MARKETVIEW_API_USERNAME=your_username
ARA_MARKETVIEW_API_PASSWORD=your_password
```

Add these to your `.Renviron` file or set them in your Posit Connect environment.

### Running Locally

```r
shiny::runApp()
```

---

## Usage

### Controls

**Max Date (query cutoff)**
The end date for the MarketView API data pull. Defaults to today.

**Load Data**
Triggers the data pipeline. On the first load of the day, fetches fresh data from the MarketView API and caches it locally. On subsequent clicks the same day, loads from the local cache instead of hitting the API.

**Force Refresh**
Bypasses the daily cache and forces a fresh API call. Use this if prices have been updated and you need the latest data within the same day.

**As-of Date (expiration logic)**
Determines which contract months are expired vs forward. Once data is loaded, only actual trading days are available as options. A contract is expired when the as-of date is on or after its last actual trading day.

**Min Year / Max Year**
Controls which contract years appear in the tables.

**Yield (gal/bu)**
Ethanol yield in gallons per bushel. Default is 2.9.

**Apply**
Recalculates crush spreads with updated As-of Date, Yield, or Year settings without re-fetching from the API.

---

## Crush Spread Calculation

```
Crush Spread = Ethanol Price - (Corn Price / 100 / Yield)
```

- **Ethanol Price** — GCU contract closing price ($/gallon)
- **Corn Price** — ZC contract closing price (cents/bushel)
- **Yield** — gallons per bushel (default 2.9)

For **expired months**, both prices are the full-month average of all daily closes within the contract month.

For **active forward months**, the latest available closing price is used for both.

---

## Corn Contract Mapping

| Ethanol Month | Corn Contract |
|---|---|
| January (F), February (G) | March corn (H) |
| March (H), April (J) | May corn (K) |
| May (K), June (M) | July corn (N) |
| July (N), August (Q) | September corn (U) |
| September (U), October (V), November (X) | December corn (Z) |
| December (Z) | March corn next year (H) |

---

## Data Caching

To minimize API calls, raw data is cached in `current-day-crush-data/` in the app directory:

- `ethanol_df.rds` — raw ethanol contract price history
- `corn_df.rds` — raw corn contract price history
- `cache_date.txt` — date the cache was last written
- `fetch_timestamp.txt` — exact timestamp of the last API fetch

The cache is wiped and rebuilt on the first Load Data click of each day, or any time Force Refresh is clicked. The last fetch timestamp is always displayed below the Load Data button.

**Add this folder to `.gitignore`:**

```
current-day-crush-data/
```

---

## Deployment (Posit Connect)

1. Set `ARA_MARKETVIEW_API_USERNAME` and `ARA_MARKETVIEW_API_PASSWORD` as environment variables in the Connect environment
2. Ensure the app has write permissions to its working directory (for the cache folder)
3. Deploy all four `.R` files together
