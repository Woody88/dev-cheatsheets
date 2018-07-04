module App.Slot 
    ( Slots
    , AppSlots
    , module Slot
    )
    where
 
import Component (CompnentSlots, _navbar, navbarComponent) as Slot
import Page (PageSlots, _homePage, homeComponent, _notfoundPage, notfoundComponent, _cheatsheetPage, cheatsheetComponent) as Slot
import Type.Row (type (+))

type AppSlots r = Slot.PageSlots + Slot.CompnentSlots + r

type Slots = AppSlots + ()
