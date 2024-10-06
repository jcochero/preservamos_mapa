# Mapa de PreserVamos

## Ciencia ciudadana en ambientes acuáticos de Argentina

Esta es una aplicación web interactiva creada con R y Shiny que visualiza datos sobre la calidad de los ríos en diferentes regiones de Argentina del proyecto [PreserVamos](https://www.preservamos.ar).

### ¿Qué hace?

En el mapa se muestra el Índice de Calidad del hábitat acuático de PreserVamos en distintos puntos. A medida que mueves el mapa, o que aumentas o disminuyes el zoom, se vuelve a calcular el índice incluyendo los puntos que caen en el visor.

A la derecha se muestra un histograma que te dará una idea del estado de la zona que tienes seleccionada en el mapa. Colores como azul o verde indican calidades buenas/muy buenas, mientras que colores como el rojo indican calidades malas (como un semáforo!).

Los usuarios pueden filtrar y explorar los datos según ecorregiones, tipos de ríos y rangos de fechas, además de analizar correlaciones entre los índices de calidad de los ríos y otras variables ambientales. La aplicación también incluye mapas dinámicos, histogramas y gráficos de correlación.

### Origen de los datos

Los datos provienen en tiempo real de la base de datos del proyecto de ciencia ciudadana PreserVamos. Los datos son abiertos, pueden ser compartidos, descargados, adaptados y reutilizados.

### ¿Hay ecorregiones sin datos?

Si, hay ecorregiones sin datos, pero puedes ser la primera persona que participe de esa ecorregión!

Sólo ve al sitio web del proyecto [PreserVamos](https://www.preservamos.ar), descarga la app y comienza tu trayectoria como científico/a ciudadano/a :)

### ¿Puedo usar este código pero vinculado a mi propia base de datos?

El código de este mapa es abierto, y está comentado para que lo puedas utilizar con tus datos. Necesitarás crear un archivo "*config.ym*l", que tenga la siguiente estructura:\

``` YARN
db_host: ip-de-tu-servidor
db_user: usuario-de-tu-base-de-datos
db_password: clave-de-tu-base-de-datos
db_name: nombre-de-tu-base-de-datos
db_table: nombre-de-la-tabla-de-datos
```

Y, en el código R, cambiar los nombres de los campos que están representados en tu base de datos. También puedes usar pedazos de este código.
