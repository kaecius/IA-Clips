#!/usr/bin/env python3

import os, clips
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
FISH_TEXT_DIMENSIONS = (FISH_DIMENSIONS[0], FISH_DIMENSIONS[1] + 34)

pygame.font.init()

FISH_FONT = pygame.font.Font('{}/fonts/OpenSansEmoji.ttf'.format(RESOURCES).format(RESOURCES), 14)
DEBUG = False
dictEmoji = {'female': '\u2640', 'male': '\u2642', 'life': '\u2665', 'strength': '\u2694', 'fight': '\u26A1', 'noBreed': '\u26D4'}
CICLO = 0

class Fish (pygame.sprite.Sprite):
    def __init__(self, movingSpriteSheet, idleSpriteSheet, posX, posY, life, sex, strength, hunger):
        super(Fish, self).__init__()
        self.moving = False
        self.movingImages = movingSpriteSheet
        self.idleImages = idleSpriteSheet
        self.index = 0
        self.life = life
        self.strength = strength
        self.sex = '\u2642' if sex == 'Macho' else '\u2640'
        self.hunger = hunger
        self.image = pygame.Surface(FISH_TEXT_DIMENSIONS, pygame.SRCALPHA)
        self.createImage()
        self.rect = pygame.Rect(posX, posY, FISH_TEXT_DIMENSIONS[0], FISH_TEXT_DIMENSIONS[1])

    def changeState(self):
        self.moving = not self.moving
        self.index = 0

    def update(self):
        self.index += 1
        nImages = len(self.movingImages) if self.moving else len(self.idleImages)

        if self.index >= nImages:
            self.index = 0
        
        self.createImage()
    
    def createImage(self):
        toBlit = self.movingImages[self.index] if self.moving else self.idleImages[self.index]
        self.image.blit(toBlit, (0, 35))
        self.image.blit(FISH_FONT.render('{} \u2665 | {} \u2668'.format(self.life, self.hunger), True, pygame.Color('white')), (0, 0))
        self.image.blit(FISH_FONT.render('{} \u2694 | {}\u26D4\u26A1'.format(self.strength, self.sex),True, pygame.Color('white')), (0, 15))

    def move(self, posX, posY):
        self.rect = pygame.Rect(posX, posY, FISH_TEXT_DIMENSIONS[0], FISH_TEXT_DIMENSIONS[1])
        self.createImage()

def handleEvent(evt):
    global RUN_MODE, DEBUG
    if evt.type == pygame.QUIT or (evt.type == pygame.KEYDOWN and evt.key == pygame.K_ESCAPE):
        pygame.quit()
        exit(0)
    elif evt.type == pygame.KEYDOWN:
        if not RUN_MODE and evt.key == pygame.K_SPACE or evt.key == pygame.K_RETURN:
            pygame.display.flip()
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

def printCLIPS():
    print('Ciclo {}'.format(CICLO))
    print('Instancias')
    instance = clips.InitialInstance()
    while (True):
        instance = instance.Next()
        if (not instance):
            break
        print(instance.PPForm())
    print('Agenda')
    clips.RefreshAgenda()
    clips.PrintAgenda()
    print()

def loadCLIPS():
    clips.Load('../clips/peces.pont')
    clips.Load('../clips/peces.clp')
    clips.Load('../clips/peces.pins')
    clips.Reset()
    clips.SendCommand('(set-salience-evaluation when-activated)')
    clips.SendCommand('(set-strategy depth)')
    printCLIPS()

def stepCLIPS():
    global CICLO
    clips.SendCommand('(run 1)')
    CICLO += 1

def main():
    ########### INITIALIZATIONS ##############
    pygame.init()
    icon = pygame.image.load('{}/icon/aquarium.png'.format(RESOURCES))
    pygame.display.set_icon(icon)
    pygame.display.set_caption('Pecera CLIPS')
    screen = pygame.display.set_mode((WIDTH, HEIGHT))
    bg = pygame.image.load('{}/background.png'.format(RESOURCES)).convert()
    background = pygame.transform.scale(bg, (WIDTH, HEIGHT))
    font = pygame.font.Font('{}/fonts/OpenSansEmoji.ttf'.format(RESOURCES).format(RESOURCES), 30)
    clock = pygame.time.Clock()
    idleSprites = loadSprites('{}/sprites/cartoon_fish_06_black_idle.png'.format(RESOURCES),
                          5, 4, 9, 9, 11, 20)
    movingSprites = loadSprites('{}/sprites/cartoon_fish_06_black_swim.png'.format(RESOURCES),
                          3, 4, 10, 11, 5, 14)
    f = Fish(movingSprites, idleSprites, 300, 300, 30, 'Hembra', 5, 10)
    grupo = pygame.sprite.Group(f)

    ############## MAIN LOOP #################
    while True:
        screen.blit(background, (0,0))
        grupo.update()
        grupo.draw(screen)

        if DEBUG:
            fps = font.render('{} FPS'.format(int(clock.get_fps())), True, pygame.Color('white'))
            screen.blit(fps, (0, 0))
        
        if RUN_MODE:
            for event in pygame.event.get(): 
                handleEvent(event)
            clock.tick(CLOCK_RATE)
            pygame.display.update()
        else:
            clock.tick(0)
            event = pygame.event.wait()
            handleEvent(event)

main()
