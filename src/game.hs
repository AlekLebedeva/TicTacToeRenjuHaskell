{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Game where

import Data.Array
import Data.Foldable ( asum )
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Matrix (Matrix(nrows))


data Player = TaeTic | TaeRen| PlayerB | PlayerW | PlayerX | PlayerO deriving (Eq, Show)
type Cell = Maybe Player
data State = Menu | RunningTic | RunningRen | GameOver (Maybe Player) deriving (Eq, Show)
data Diagonal = L | R
type Board = Array (Int, Int) Cell

data Game = Game {gameBoard :: Board,
                  gamePlayer :: Player,
                  gameState :: State,
                  currCell :: (Int, Int)
                  } deriving (Eq, Show)

--размер экрана
screenWidth :: Int
screenWidth = 640
screenHeight :: Int
screenHeight = 640

--размеры поля
nTic :: Int 
nTic = 3
nRen :: Int 
nRen = 15

--размеры ячейки
cellWidthtic :: Float
cellWidthtic = fromIntegral screenWidth / fromIntegral nTic
cellHeighttic :: Float
cellHeighttic = fromIntegral screenHeight / fromIntegral nTic
cellWidthren :: Float
cellWidthren = fromIntegral screenWidth / fromIntegral nRen
cellHeightren :: Float
cellHeightren = fromIntegral screenHeight / fromIntegral nRen

--Менюшка
initialGame :: Game
initialGame = Game { gameBoard = array indexRange $ zip (range indexRange) (repeat Nothing)
                   , gamePlayer = PlayerX
                   , gameState = Menu
                   , currCell = (0, 0)
                   }
    where indexRange = ((0, 0), (0, 0))



--функция запуска игры
runningGameTic :: Game
runningGameTic = Game { gameBoard = array indexRange $ zip (range indexRange) (repeat Nothing)
                   , gamePlayer = PlayerX
                   , gameState = RunningTic
                   , currCell = (0, 0)
                   }
    where indexRange = ((0, 0), (nTic - 1, nTic - 1))

runningGameRen :: Game
runningGameRen = Game { gameBoard = array indexRange $ zip (range indexRange) (repeat Nothing)
                   , gamePlayer = PlayerB
                   , gameState = RunningRen
                   , currCell = (0, 0)
                   }
    where indexRange = ((0, 0), (nRen - 1, nRen - 1))

--цвета элементов
boardGridColor = makeColorI 255 255 255 255
playerWColor = makeColorI 0 0 255 255
playerBColor = makeColorI 255 0 0 255
playerXColor = makeColorI 255 0 0 255
playerOColor = makeColorI 0 0 255 255
tieColor = greyN 0.5

--отрисовка доски
boardAsRunningPictureTic board game = 
    case gamePlayer game of
        PlayerX -> pictures[ translate (fromIntegral screenWidth / 4.2) (fromIntegral screenHeight + 10) 
                            $ color white  $ scale 0.15 0.15 $ text "Press space button to restart"
                            , translate (fromIntegral screenWidth / 10.0) (fromIntegral screenHeight + 15)
                            $ color playerXColor $ circleSolid 10.0
                            , color playerXColor $ xCellsOfBoard board
                            , color playerOColor $ oCellsOfBoard board
                            , color boardGridColor $ boardGridTic
                            ]
        PlayerO -> pictures[ translate (fromIntegral screenWidth / 4.2) (fromIntegral screenHeight + 10) 
                            $ color white  $ scale 0.15 0.15 $ text "Press space button to restart"
                            , translate (fromIntegral screenWidth / 10.0) (fromIntegral screenHeight + 15)
                            $ color playerOColor $ circleSolid 10.0
                            , color playerXColor $ xCellsOfBoard board
                            , color playerOColor $ oCellsOfBoard board
                            , color boardGridColor $ boardGridTic
                            ]
    

boardAsRunningPictureRen board game = 
    case gamePlayer game of
            PlayerB -> pictures[translate (fromIntegral screenWidth / 4.2) (fromIntegral screenHeight + 10) 
                                $ color white  $ scale 0.15 0.15 $ text "Press space button to restart"
                                , translate (fromIntegral screenWidth / 10.0) (fromIntegral screenHeight + 15)
                                $ color playerBColor $ circleSolid 10.0
                                , color playerBColor $ bCellsOfBoard board
                                , color playerWColor $ wCellsOfBoard board
                                , color boardGridColor $ boardGridRen
                                ]
            PlayerW -> pictures[translate (fromIntegral screenWidth / 4.2) (fromIntegral screenHeight + 10) 
                                $ color white  $ scale 0.15 0.15 $ text "Press space button to restart"
                                , translate (fromIntegral screenWidth / 10.0) (fromIntegral screenHeight + 15)
                                $ color playerWColor $ circleSolid 10.0
                                , color playerBColor $ bCellsOfBoard board
                                , color playerWColor $ wCellsOfBoard board
                                , color boardGridColor $ boardGridRen
                                ]
    

outcomeColor (Just PlayerX) = playerXColor
outcomeColor (Just PlayerO) = playerOColor
outcomeColor (Just PlayerB) = playerBColor
outcomeColor (Just PlayerW) = playerWColor
outcomeColor (Just TaeRen) = tieColor
outcomeColor (Just TaeTic) = tieColor

snapPictureToCellTic picture (row, column) = translate x y picture
    where x = fromIntegral column * cellWidthtic + cellWidthtic * 0.5
          y = fromIntegral row * cellHeighttic + cellHeighttic * 0.5

snapPictureToCellRen picture (row, column) = translate x y picture
    where x = fromIntegral column * cellWidthren + cellWidthren * 0.5
          y = fromIntegral row * cellHeightren + cellHeightren * 0.5

--отрисовка фишек
bCell :: Picture
bCell = circleSolid 15.0

wCell :: Picture
wCell = circleSolid 15.0

xCell :: Picture
xCell = pictures[ rotate 45.0 $ rectangleSolid side 10.0
                , rotate (-45.0) $ rectangleSolid side 10.0 
                ]
    where side = min cellWidthtic  cellHeighttic * 0.75

oCell :: Picture
oCell = thickCircle radius 10.0
    where radius = min cellWidthtic cellHeighttic * 0.25

--отрисовка фишек на поле
cellsOfBoardTic :: Board -> Cell -> Picture -> Picture
cellsOfBoardTic board cell cellPicture =
    pictures
    $ map (snapPictureToCellTic cellPicture . fst)
    $ filter (\(_, e) -> e == cell)
    $ assocs board

cellsOfBoardRen :: Board -> Cell -> Picture -> Picture
cellsOfBoardRen board cell cellPicture =
    pictures
    $ map (snapPictureToCellRen cellPicture . fst)
    $ filter (\(_, e) -> e == cell)
    $ assocs board

xCellsOfBoard :: Board -> Picture
xCellsOfBoard board = cellsOfBoardTic board (Just PlayerX) xCell

oCellsOfBoard :: Board -> Picture
oCellsOfBoard board = cellsOfBoardTic board (Just PlayerO) oCell

bCellsOfBoard :: Board -> Picture
bCellsOfBoard board = cellsOfBoardRen board (Just PlayerB) bCell

wCellsOfBoard :: Board -> Picture
wCellsOfBoard board = cellsOfBoardRen board (Just PlayerW) wCell

--отрисовка сетки
boardGridTic :: Picture
boardGridTic =
    pictures
    $ concatMap (\i -> [ line [ (i * cellWidthtic, 0.0)
                              , (i * cellWidthtic, fromIntegral screenHeight)
                              ]
                       , line [ (0.0,                      i * cellHeighttic)
                              , (fromIntegral screenWidth, i * cellHeighttic)
                              ]
                       ])
      [0.0 .. fromIntegral nTic]

boardGridRen :: Picture
boardGridRen =
    pictures
    $ concatMap (\i -> [ line [ (i * cellWidthren, 0.0)
                              , (i * cellWidthren, fromIntegral screenHeight)
                              ]
                       , line [ (0.0,                      i * cellHeightren)
                              , (fromIntegral screenWidth, i * cellHeightren)
                              ]
                       ])
      [0.0 .. fromIntegral nRen]
            

boardAsPicture board winner =
    case winner of
        Just PlayerX -> pictures [ xCellsOfBoard board
                      , oCellsOfBoard board
                      , boardGridTic
                        ]
        Just PlayerO -> pictures [ xCellsOfBoard board
                 , oCellsOfBoard board
                 , boardGridTic
                 ]
        Just PlayerW -> pictures [ bCellsOfBoard board
                 , wCellsOfBoard board
                 , boardGridRen
                ]
        Just PlayerB -> pictures [ bCellsOfBoard board
                 , wCellsOfBoard board
                 , boardGridRen
                ]
        Just TaeRen -> pictures [ bCellsOfBoard board
                    , wCellsOfBoard board
                    , boardGridRen
                    ]
        Just TaeTic -> pictures [ xCellsOfBoard board
                    , oCellsOfBoard board
                    , boardGridTic
                    ]

boardAsMenuPicture board =
    pictures [ translate (fromIntegral screenWidth / 3) (fromIntegral screenHeight / 4)
            $ color playerXColor $ xCell
            , translate (fromIntegral screenWidth / 1.6) (fromIntegral screenHeight / 4)
            $ color playerOColor $ oCell
            , translate (fromIntegral screenWidth / 3) (fromIntegral screenHeight / 1.5)
            $ color playerXColor $ circleSolid 30.0
            , translate (fromIntegral screenWidth / 1.6) (fromIntegral screenHeight / 1.5)
            $ color playerOColor $ circleSolid 30.0
            , translate (fromIntegral screenWidth / 4) (fromIntegral screenHeight / 2 - 30) 
            $ color white  $ scale 0.15 0.15 $ text "Press down button to play TicTacToe"
            , translate (fromIntegral screenWidth / 4) (fromIntegral screenHeight / 2) 
            $ color white $ scale 0.15 0.15 $ text "Press up button to play Renju"
            , translate (fromIntegral screenWidth / 4) (fromIntegral screenHeight / 1.1) 
            $ color white $ scale 0.15 0.15 $ text "Press esc button to exit"
            ]

isCoordCorrectTic = inRange ((0, 0), (nTic - 1, nTic - 1))
isCoordCorrectRen = inRange ((0, 0), (nRen - 1, nRen - 1))

switchPlayer game =
    case gamePlayer game of
        PlayerX -> do
            game { gamePlayer = PlayerO }
        PlayerO -> do
            game { gamePlayer = PlayerX }
        PlayerB -> do
            game { gamePlayer = PlayerW }
        PlayerW -> do
            game { gamePlayer = PlayerB }

full :: [Cell] -> Maybe Player
full (cell@(Just player):cells) | all (== cell) cells = Just player
full _                                                = Nothing  

--определение выигрыша
winner :: Game -> Board -> Maybe Player
winner game board = 
    case gameState game of
        RunningTic -> asum $ map full $ rows ++ cols ++ diags
        RunningRen -> asum $ map full $ rowsL ++ colsL ++ diagsR ++ diagsL ++ rowsR ++ colsR
    where cur = currCell game
          rowsL  = [[board ! (i,j) | i <- [fst cur]
                          , j <- [(snd cur) .. snd cur + 4]
                          , j >= 0 && j <= (nRen-1)]]
          colsL  = [[board ! (i,j) | i <- [(fst cur) .. (fst cur + 4)]
                          , j <- [snd cur]
                          , i >= 0 && i <= nRen-1]]
          rowsR  = [[board ! (i,j) | i <- [fst cur]
                          , j <- [((snd cur))-4 .. ((snd cur))]
                          , j >= 0 && j <= nRen-1]]
          colsR  = [[board ! (i,j) | i <- [(fst cur)-4 .. (fst cur)]
                          , j <- [snd cur]
                          , i >= 0 && i <= nRen-1]]
          diagsR = [[board ! (i,i) | i <- [(fst cur)..fst cur + 4]]
                  ,[board ! (i,j) | i <- [(fst cur)..fst cur + 4], let j = nRen - i - 1 ]]
          diagsL = [[board ! (j,j) | j <- [(snd cur) + 4..snd cur]]
                  ,[board ! (i,j) | j <- [(snd cur) + 4..snd cur], let i = nRen - j - 1 ]]
          rows  = [[board ! (i,j) | i <- [0..nTic-1]] | j <- [0..nTic-1]]
          cols  = [[board ! (j,i) | i <- [0..nTic-1]] | j <- [0..nTic-1]]
          diags = [[board ! (i,i) | i <- [0..nTic-1]]
                  ,[board ! (i,j) | i <- [0..nTic-1], let j = nTic-1-i ]]

countCells :: Cell -> Board -> Int 
countCells cell = length . filter ((==) cell) . elems

--определение конца игры
checkGameOver game 
    | Just p <- winner game board =
        game { gameState = GameOver $ Just p }
    | countCells Nothing board == 0 =
        case gameState game of
            RunningTic -> game { gameState = GameOver $ Just TaeTic }
            RunningRen -> game { gameState = GameOver $ Just TaeRen }
    | otherwise = game
    where board = gameBoard game

--определение кто ходит
playerTurnTic :: Game -> (Int, Int) -> Game
playerTurnTic game cellCoord
    | isCoordCorrectTic cellCoord && board ! cellCoord == Nothing =
        checkGameOver
        $ switchPlayer
        $ game { gameBoard = board // [(cellCoord, Just player)],
                 currCell =  (0,0)}
    | otherwise = game
    where board = gameBoard game
          player = gamePlayer game

playerTurnRen :: Game -> (Int, Int) -> Game
playerTurnRen game cellCoord
    | isCoordCorrectRen cellCoord && board ! cellCoord == Nothing =
        checkGameOver
        $ switchPlayer
        $ game { gameBoard = board // [(cellCoord, Just player)],
                 currCell =  cellCoord}
    | otherwise = game
    where board = gameBoard game
          player = gamePlayer game

mousePosAsCellCoordTic :: (Float, Float) -> (Int, Int)
mousePosAsCellCoordTic (x,y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeighttic )
                            , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidthtic )
                            )

mousePosAsCellCoordRen :: (Float, Float) -> (Int, Int)
mousePosAsCellCoordRen (x,y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeightren )
                            , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidthren )
                            )


boardAsGameOverPicture winner board = color (outcomeColor winner) (boardAsPicture board winner)
gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
    where frame = case gameState game of
                    Menu -> boardAsMenuPicture (gameBoard game)
                    RunningTic -> boardAsRunningPictureTic (gameBoard game) game
                    RunningRen -> boardAsRunningPictureRen (gameBoard game) game
                    GameOver winner -> boardAsGameOverPicture winner (gameBoard game)
                    
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game = 
    case gameState game of
        Menu -> initialGame
        RunningTic -> playerTurnTic game $ mousePosAsCellCoordTic mousePos
        RunningRen -> playerTurnRen game $ mousePosAsCellCoordRen mousePos
        GameOver _ -> initialGame

transformGame (EventKey (SpecialKey KeySpace) Up _ _)  game =
    case gameState game of
        Menu -> initialGame
        RunningTic -> initialGame
        RunningRen -> initialGame
        GameOver _ -> initialGame

transformGame (EventKey (SpecialKey KeyUp) Down _ _)  game =
    case gameState game of
        Menu -> runningGameRen
        RunningTic -> game
        RunningRen -> game
        GameOver _ -> initialGame

transformGame (EventKey (SpecialKey KeyDown) Down _ _)  game =
    case gameState game of
        Menu -> runningGameTic
        RunningTic -> game
        RunningRen -> game
        GameOver _ -> initialGame

transformGame _ game = game
