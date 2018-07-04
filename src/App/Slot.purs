module App.Slot 
    ( Slots
    , AppSlots
    , module Slot
    )
    where
 
import Type.Row (type (+))
import Page (PageSlots, _homePage, homeComponent, _notfoundPage, notfoundComponent) as Slot
import Component (CompnentSlots, _navbar, navbarComponent) as Slot

type AppSlots r = Slot.PageSlots + Slot.CompnentSlots + r

type Slots = AppSlots + ()
