import PySimpleGUI as sg
from lexer import init_lexer
from parser import init_parser
from interp import *


sg.theme('DarkGrey8')
layout_l = [
    [sg.Text(k='file_path', text='')],
    [sg.Sizer(0,6)],
    [sg.Multiline(k='input', s=(116,40), expand_y=True, horizontal_scroll=True, font='Consolas 11', focus=True)]
]
layout_r = [
    [sg.Push(), sg.Button('Run'), sg.Button('Clear')],
    [sg.Multiline(k='output', s=(52,40), expand_x=True, expand_y=True, font='Consolas 10', write_only=True, autoscroll = True, reroute_stdout=True)]
]

layout = [
    [sg.Menu([['File', ['Open File...']]], background_color = 'ghost white', text_color = 'black' )],
    [sg.Col(layout_l, expand_y=True), sg.Col(layout_r, expand_x=True, expand_y=True)]
    ]


# Create the Window
window = sg.Window('Proyecto Final', layout, resizable=True, finalize=True)
window.Maximize()
# Enable the undo mechanism
input_window = window['input']
input_widget = input_window.Widget
input_widget.configure(undo=True)


# HOTKEYS
window.bind('<Control-r>', 'Run')
input_widget.bind('<Control-Shift-Key-Z>', lambda event, text=input_widget:redo(event, text))
input_widget.bind('<Control-y>', lambda event, text=input_widget:redo(event, text))


def redo(event, text):
    try:
        text.edit_redo()
    except:
        pass

def open_file():
    filename = sg.popup_get_file('file to open', no_window=True)
    if filename:
        f = open(filename,'rt',encoding='utf-8')
        str_ = f.read()
        f.close()
        window['file_path'].update(value=filename+':')
        input_window.update(value=str_)

def run():
    input_value = values['input']
    window['output'].print("> ")
    init_lexer(input_value)
    print(interp(init_parser(input_value)))
    input_window.set_focus()


# Event Loop to process "events" and get the "values" of the inputs
while True:
    event, values = window.read()
    match event:
        case sg.WIN_CLOSED:
            break
        case 'Open File...':
            open_file()
        case 'Clear':
            window['output'].update(value='')
            input_window.set_focus()
        case 'Run':
            run()

window.close()