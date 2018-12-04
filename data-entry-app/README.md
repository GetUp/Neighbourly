# Data entry app

Simple data entry app that mimics walksheet format.  Data is persisted to the `neighbourly` schema in data db, via a lambda that lives in `turf-sls`.

## Setup

- `cd data-entry-app`
- `npm install`

You'll probably also want to sort out [editor integration](https://github.com/avh4/elm-format#editor-integration).

## Development

- `NODE_ENV=development bundle exec puma -p 3000` (from neighbourly top-level directory)
- `npm start`
- visit http://localhost:3000/data_entry
