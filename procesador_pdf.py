import pdfkit
options = {
    'page-size': 'Letter',
    'margin-top': '0.75in',
    'margin-right': '0.75in',
    'margin-bottom': '0.75in',
    'margin-left': '0.75in',
    'encoding': "UTF-8",
    'no-outline': None
}
archivo = input("Ingresa nombre del archivo: ")

pdfkit.from_file(archivo + ".html", archivo +".pdf", options=options)
