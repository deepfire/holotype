{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
module ThinErr (plugin) where
import GhcPlugins
import Debug.Trace

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = installPRA
  , installCoreToDos   = installCTD
  }

installPRA :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
installPRA _ ModSummary{..} m@HsParsedModule{..} = do
  liftIO $ putStrLn $ "installPRA ---------------------------------------------------------------------- "
  pure m

installCTD :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
installCTD _ todo = do
  liftIO $ putStrLn $ "installCTD ---------------------------------------------------------------------- "
  return (CoreDoPluginPass "Say name" pass : todo)

pass :: ModGuts -> CoreM ModGuts
pass mg@ModGuts{..} = do
  he <- getHscEnv
  let printBind :: CoreBind -> CoreM CoreBind
      printBind bndr@(NonRec b _) = do
        putMsgS $ "Non-recursive binding named " ++ showSDoc (hsc_dflags he) (ppr b)
        return bndr 
      printBind bndr = return bndr
  bindsOnlyPass (mapM printBind) mg
