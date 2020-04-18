#!/usr/bin/env python3

import os, re, random, clips
os.environ['PYGAME_HIDE_SUPPORT_PROMPT'] = 'hide'

# Import pygame after setting env variable to hide welcome message
import pygame

WIDTH = 1200
HEIGHT = 720
CLOCK_RATE = 10
RESOURCES = 'res'
RUN_MODE = False # Run mode false -> step by step running
                 # Run mode true -> autorun
FISH_DIMENSIONS = (97, 63)
FOOD_DIMENSIONS = (50, 50)
FOOD_TEXT_DIMENSIONS = (FOOD_DIMENSIONS[0], FOOD_DIMENSIONS[1] + 34)
FISH_TEXT_DIMENSIONS = (FISH_DIMENSIONS[0], FISH_DIMENSIONS[1] + 34)
FISHBOWL_DIMENSIONS = (0, 0)

FISH_COLORS = ['black', 'blue', 'green', 'purple', 'red']
SPRITE_SHEET_FILENAME = 'cartoon_fish_06_{}_{}.png'

pygame.font.init()

FISH_FONT = pygame.font.Font('{}/fonts/OpenSansEmoji.ttf'.format(RESOURCES).format(RESOURCES), 14)
DEBUG = False
dictEmoji = {'female': '\u2640', 'male': '\u2642', 'life': '\u2665', 'strength': '\u2694', 'fight': '\u26A1', 'noBreed': '\u26D4'}
PASO = 0
RELOJ = 0
FISH_DICT = {}
FISH_ANIMATION_DICT = {}
FOOD = None

class Fish (pygame.sprite.Sprite):
    def __init__(self, movingSpriteSheet, idleSpriteSheet, posX, posY, life, sex, strength, hunger, canBreed):
        super(Fish, self).__init__()
        self.moving = False
        self.posX= posX
        self.posY = posY
        self.life = life
        self.strength = strength
        self.sex = '\u2642' if sex == 'Macho' else '\u2640'
        self.hunger = hunger
        self.canBreed = '\u26D4' if canBreed > 0 else ''

        self.movingImages = loadSprites(movingSpriteSheet, 3, 4, 10, 11, 5, 14)
        self.idleImages = loadSprites(idleSpriteSheet, 5, 4, 9, 9, 11, 20)
        self.index = 0
        self.createImage()

    def changeState(self):
        self.moving = not self.moving
        self.index = 0

    def update(self):
        self.index += 1
        nImages = len(self.movingImages) if self.moving else len(self.idleImages)

        if self.index >= nImages:
            self.index = 0
        
        self.createImage()

    def updateAttributes(self, posX, posY, life, hunger, canBreed):
        self.posX = posX
        self.posY = posY
        self.life = life
        self.hunger = hunger
        self.canBreed = '\u26D4' if canBreed > 0 else ''
        self.createImage()
    
    def createImage(self):
        self.rect = pygame.Rect((self.posX, self.posY), FISH_TEXT_DIMENSIONS)
        self.image = pygame.Surface(FISH_TEXT_DIMENSIONS, pygame.SRCALPHA)
        toBlit = self.movingImages[self.index] if self.moving else self.idleImages[self.index]
        self.image.blit(toBlit, (0, 35))
        self.image.blit(FISH_FONT.render('{} \u2665 | {} \u2668'.format(self.life, self.hunger), True, pygame.Color('black')), (0, 0))
        self.image.blit(FISH_FONT.render('{} \u2694 | {}{}'.format(self.strength, self.sex, self.canBreed),True, pygame.Color('black')), (0, 15))

class Food(pygame.sprite.Sprite):
    def __init__(self, imagePath, posX, posY, units, regen):
        super(Food, self).__init__()
        self.baseImage = pygame.transform.scale(pygame.image.load(imagePath).convert_alpha(), FOOD_DIMENSIONS)
        self.rect = pygame.Rect((posX, posY), FOOD_TEXT_DIMENSIONS)
        self.regen = '\u267B {}'.format(regen)
        self.grp = pygame.sprite.Group([self])
        self.updateQty(units)

    def updateQty(self, qty):
        if qty > 0:
            self.supplies = ''
            self.units = qty
        else:
            if qty == 0:
                self.units = qty
            self.supplies = '| \u26FD'
        
        self.createImage()

    def createImage(self):
        self.image = pygame.Surface(FOOD_TEXT_DIMENSIONS, pygame.SRCALPHA)
        self.image.blit(self.baseImage, (0, 35))
        self.image.blit(FISH_FONT.render('{} {}'.format(self.units, self.supplies), True, pygame.Color('black')), (0, 0))
        self.image.blit(FISH_FONT.render('{}'.format(self.regen), True, pygame.Color('black')), (0, 15))
    def draw(self, surface):
        self.createImage()
        self.grp.draw(surface)

def handleEvent(evt):
    global RUN_MODE, DEBUG
    if evt.type == pygame.QUIT or (evt.type == pygame.KEYDOWN and evt.key == pygame.K_ESCAPE):
        pygame.quit()
        exit(0)
    elif evt.type == pygame.KEYDOWN:
        if not RUN_MODE and evt.key == pygame.K_SPACE or evt.key == pygame.K_RETURN:
            stepCLIPS()
            printCLIPS()
            return True
        elif evt.key == pygame.K_d:
            DEBUG = not DEBUG
        elif evt.key == pygame.K_m:
            RUN_MODE = not RUN_MODE

def loadSprites(filename, nrows, ncols, marginX, paddingX,  marginY, paddingY):
        spriteSheet = pygame.image.load(filename).convert_alpha()
        sheetRect = spriteSheet.get_rect()

        spriteX = ( sheetRect.size[0] - 2 * marginX - (ncols - 1) * paddingX ) / ncols
        spriteY = ( sheetRect.size[1] - 2 * marginY - (nrows - 1) * paddingY ) / nrows

        spriteImages = []
        for i in range(nrows):
            for j in range(ncols):
                x = marginX + j * (spriteX + paddingX)
                y = marginY + i * (spriteY + paddingY)
                spriteRect = pygame.Rect(x, y, spriteX, spriteY)
                spriteImages.append(pygame.transform.scale(spriteSheet.subsurface(spriteRect), FISH_DIMENSIONS))

        return spriteImages

def mapCoords(posX, posY):
    mappedX = posX / FISHBOWL_DIMENSIONS[0] * WIDTH
    mappedY = posY / FISHBOWL_DIMENSIONS[1] * HEIGHT
    return mappedX, mappedY

def printCLIPS():
    print(chr(27) + "[2J")
    print('PASO {}'.format(PASO))
    print('Instancias')
    print(getInstanceString())
    print('Facts')
    clips.PrintFacts()
    print('\nAgenda')
    clips.RefreshAgenda()
    clips.PrintAgenda()
    print()

def getInstanceString():
    instanceStr = ''
    instance = clips.InitialInstance()
    while (True):
        instance = instance.Next()
        if (not instance):
            break
        instanceStr += '{}\n'.format(instance.PPForm())
    return instanceStr

def loadCLIPS():
    clips.Load('src/clips/peces.pont')
    clips.Load('src/clips/peces.clp')
    clips.Load('src/clips/peces.pins')
    clips.Reset()
    clips.SendCommand('(set-salience-evaluation when-activated)')
    clips.SendCommand('(set-strategy depth)')
    printCLIPS()
    loadFishFromCLIPS(True)

def stepCLIPS():
    global PASO
    clips.SendCommand('(run 1)')
    PASO += 1
    loadFishFromCLIPS(False)
    printCLIPS()

def loadFishFromCLIPS(initial):
    global RELOJ, FISH_DICT, FISH_ANIMATION_DICT, FOOD, FISHBOWL_DIMENSIONS
    instances = getInstanceString().splitlines()
    for i in instances:
        if 'Comida' in i:
            qty =  int(re.search('\(Cantidad (-?\d+)\)', i).group(1))
            if initial:
                posX =  int(re.search('\(PosX (\d+)\)', i).group(1))
                posY =  int(re.search('\(PosY (\d+)\)', i).group(1))
                posX, posY = mapCoords(posX, posY)
                lifeGain = int(re.search('\(IncrementoVida (\d+)\)', i).group(1))
                FOOD = Food('{}/food.png'.format(RESOURCES), posX, posY, qty, lifeGain)
            else:
                FOOD.updateQty(qty)
        elif initial and 'PeceraStandard' in i:
            bowlW = int(re.search('\(Ancho (\d+)\)', i).group(1))
            bowlH = int(re.search('\(Alto (\d+)\)', i).group(1))
            FISHBOWL_DIMENSIONS = (bowlW, bowlH)
        elif 'Pez' in i:
            posX =  int(re.search('\(PosX (\d+)\)', i).group(1))
            posY =  int(re.search('\(PosY (\d+)\)', i).group(1))
            posX, posY = mapCoords(posX, posY)
            hunger = int(re.search('\(Hambre (\d+)\)', i).group(1))
            canBreed = int(re.search('\(PeriodoNoReproducirse (\d+)\)', i).group(1))
            life = int(re.search('\(Vida (-?\d+)\)', i).group(1))
            fishName = re.search('\[(.+)\]', i).group(1)
            try:
                FISH_DICT[fishName].updateAttributes(posX, posY, life, hunger, canBreed)
            except KeyError:
                fishColor = random.choice(FISH_COLORS)
                sex = re.search('\(Sexo (Hembra|Macho)\)', i).group(1)
                strength = int(re.search('\(Fuerza (\d+)\)', i).group(1))
                FISH_DICT[fishName] = Fish('{}/sprites/{}'.format(RESOURCES, SPRITE_SHEET_FILENAME.format(fishColor, 'swim')),
                                           '{}/sprites/{}'.format(RESOURCES, SPRITE_SHEET_FILENAME.format(fishColor, 'idle')),
                                           posX, posY, life, sex, strength, hunger, canBreed)
                FISH_ANIMATION_DICT[fishName] = pygame.sprite.Group(FISH_DICT[fishName])
        elif 'Mundo_peces' in i:
            RELOJ = int(re.search('\(Reloj (\d+)\)', i).group(1))

def draw(screen, background, timerFont, clock):
    screen.blit(background, (0,0))
    timer = timerFont.render('Reloj {}'.format(RELOJ), True, pygame.Color('white'))
    screen.blit(timer, (WIDTH - timer.get_rect().size[0], 0))

    if DEBUG:
        fps = timerFont.render('{} FPS'.format(int(clock.get_fps())), True, pygame.Color('white'))
        screen.blit(fps, (0, 0))

    FOOD.draw(screen)
    for i in FISH_ANIMATION_DICT.values():
        i.draw(screen)
    pygame.display.flip()

def main():
    ########### INITIALIZATIONS ##############
    pygame.init()
    icon = pygame.image.load('{}/icon/aquarium.png'.format(RESOURCES))
    pygame.display.set_icon(icon)
    pygame.display.set_caption('Pecera CLIPS')
    screen = pygame.display.set_mode((WIDTH, HEIGHT))
    bg = pygame.image.load('{}/background.png'.format(RESOURCES)).convert()
    background = pygame.transform.scale(bg, (WIDTH, HEIGHT))
    timerFont = pygame.font.Font('{}/fonts/OpenSansEmoji.ttf'.format(RESOURCES).format(RESOURCES), 30)
    clock = pygame.time.Clock()
    loadCLIPS()

    draw(screen, background, timerFont, clock)

    ############## MAIN LOOP #################
    while True:
        
        if RUN_MODE:
            for event in pygame.event.get(): 
                handleEvent(event)
            clock.tick(CLOCK_RATE)
            stepCLIPS()
            draw(screen, background, timerFont, clock)
            pygame.display.update()
        else:
            clock.tick(0)
            event = pygame.event.wait()
            if handleEvent(event):
                draw(screen, background, timerFont, clock)

main()
