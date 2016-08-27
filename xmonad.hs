import qualified Data.Map as M
import Graphics.X11.Types
import XMonad hiding ( (|||) )
import XMonad.Config.Desktop(desktopLayoutModifiers)
import XMonad.Config.Xfce
import XMonad.Layout(Tall(..))
import XMonad.Layout.LayoutCombinators( JumpToLayout(..), (|||) )
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Reflect(reflectHoriz)
import qualified XMonad.StackSet as W

-- my layouts
import XMonad.Layout.Reading

myFocusedBorderColor = "#c59d05"

defKeys    = keys xfceConfig
delKeys x  = foldr M.delete           (defKeys x) (toRemove x)
newKeys x  = foldr (uncurry M.insert) (delKeys x) (toAdd    x)
-- remove some of the default key bindings
toRemove XConfig{modMask = modm} =
    [ (modm              , xK_j     )
    , (modm              , xK_k     )
    , (modm              , xK_p     )
    , (modm .|. shiftMask, xK_p     )
    , (modm .|. shiftMask, xK_q     )
    , (modm              , xK_q     )
    , (modm .|. shiftMask, xK_Return)
    , (modm              , xK_w     )
    , (modm              , xK_e     )
    , (modm              , xK_r     )
    , (modm              , xK_t     )
    , (modm              , xK_space )
    ]

-- add some new key bindings
toAdd XConfig{modMask = modm} =
    [ ((modm,       xK_Escape), spawn "layout_switch.sh")
    , ((modm,       xK_c), kill)
    , ((mod1Mask,   xK_F4), kill)
    , ((modm,       xK_p), withFocused $ windows . W.sink) -- push back into tiling
    , ((modm,       xK_Down), sendMessage Shrink)
    , ((modm,       xK_Up), sendMessage Expand) ]
    ++
    [ ((modm .|. m, k), jumpToLayout l h u)
        | (k,l,u) <- [(xK_KP_Insert, "Reading", True),
                      (xK_KP_Begin, "Full", False),
                      (xK_KP_Left, "Tall", False),
                      (xK_KP_Up, "Fat", False),
                      (xK_KP_Right, "TallRight", False),
                      (xK_KP_Down, "FatTop", False)]
        , (m,h) <- [(0, True), (shiftMask, False)] ]
    ++
    -- mod-{y,u,i} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{y,u,i} %! Move client to screen 1, 2, or 3
    [((modm .|. m, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_y, xK_u, xK_i] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

jumpToLayout layout wantPanel useHide = do
    windows W.swapMaster
    setRespectStruts wantPanel
    hidePanel (not wantPanel && useHide)
    sendMessage (JumpToLayout layout)
  where
    hidePanel s = spawn $ "xfconf-query -c xfce4-panel -p /panels/panel-0/autohide -s " ++
        if s then "true" else "false"
    setRespectStruts q = if q
        then sendMessage $ SetStruts [minBound .. maxBound] []
        else sendMessage $ SetStruts [] [minBound .. maxBound]

myLayoutHook = desktopLayoutModifiers $ noBorders $ 
    tiled |||
    Full |||
    Reading (6/100) (3/4) |||
    renamed [Replace "Fat"] (Mirror tiled) |||
    renamed [Replace "TallRight"] tiledSmallRight |||
    renamed [Replace "FatTop"] (Mirror tiledSmallRight)
  where
    tiled = Tall 1 (3/100) (4/7)
    tiledSmallRight = reflectHoriz $ Tall 1 (3/100) (3/7)

main = xmonad xfceConfig
    { modMask = mod4Mask
    , startupHook = setWMName "LG3D" -- mysterious Java fix
    , layoutHook = myLayoutHook
    , keys = newKeys
    , focusedBorderColor = myFocusedBorderColor
    }
