module Database.Groceries.Migrate ( migrate ) where

import Database.Beam
import Database.Beam.Migrate

import Database.Groceries

migrate :: WithDB r m => m ()
migrate = void $ runDB $
  runMigrationSteps 0 Nothing migrationSteps (\_n _comment -> executeMigration runNoReturn)
