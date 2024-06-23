# EPL Standings

## Overview
This project retrieves and displays English Premier League standings using R. It provides insights into team performance, points, and various statistics based on user-specified date and season.

## Features
- Generates standings based on a specified date and season.
- Calculates various statistics including points per match, goals scored, and goals allowed.
- Identifies team records, home and away performance, and recent performance.

## Installation
To use this project, you'll need R installed on your system. Clone this repository to get started.

```bash
git clone https://github.com/hodubena/EPL_Standings.git
cd EPL_Standings
```

## Usage
1. **Option 1: Shiny UI**
   - Access a deployed version of this project via Shiny. Check it out [here](https://hodubena.shinyapps.io/EPLStandingsPro/).

2. **Option 2: Run Locally**
   - Open `EPL_Standings.R` in your preferred R environment.
   - Modify the EPL_Standings function call to specify your desired date and season:
   ```r
   EPL_Standings("03/07/2022", "2021/22")
   ```
   - Run the script to generate the EPL standings for the specified date and season.

## License
This project is licensed under the MIT License. See the [LICENSE](./LICENSE) file for more details.

## Contributing
Contributions are welcome! If you have suggestions, feature requests, or find bugs, please [open an issue](https://github.com/hodubena/EPL_Standings/issues) or submit a pull request.
