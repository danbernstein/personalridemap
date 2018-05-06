# Personalized Bikeshare Visualizations 
This experimental Shiny app allows individuals to visualize and customize their personal Capital Bikeshare (CaBi) ridership data. While webscraping would be a technically-feasible option that would reduce the burden on the user, this app is in compliance with CaBi's data license agreement. 

To use the app, individuals need to:
  1. log onto the CaBi website using their login credentials
  2. navigate to the "trips" section using the sidebar
  3. click "export trips"
  4. select a date range of trips they would like to visualize (note that CaBi limits the date range to less than 16 months)
  5. click "export" and save the rendered page as a basic html file. This file will be uploaded to the Shiny app for visualization
