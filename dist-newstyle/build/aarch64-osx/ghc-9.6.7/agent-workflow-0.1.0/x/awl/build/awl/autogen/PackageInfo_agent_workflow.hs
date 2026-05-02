{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_agent_workflow (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "agent_workflow"
version :: Version
version = Version [0,1,0] []

synopsis :: String
synopsis = "Big-step interpreter and parser for the Agent Workflow Language"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
