module Text.Routing.Happstack where

import            Data.Data
import            Happstack.Server

import            Text.Routing.Core

routeMethod :: Method -> Router Method a a
routeMethod meth = routeData parse gen
  where parse methodData = if methodData == Just meth
                           then methodData
                           else Nothing

        gen methodData = if methodData == Nothing
                         then Just meth
                         else methodData

get :: (Typeable a, Data b) => (a -> b) -> Router Method a b
get constructor = routeMethod GET >>> end constructor

post :: (Typeable a, Data b) => (a -> b) -> Router Method a b
post constructor = routeMethod POST >>> end constructor

delete :: (Typeable a, Data b) => (a -> b) -> Router Method a b
delete constructor = routeMethod DELETE >>> end constructor


