/*************************************************
Diego Maquedano Pedrero
Creación Base de Datos Tiendas Aplicaciones
*************************************************/

/* Eliminar base de datos si existía previamente */
DROP DATABASE IF EXISTS TiendasAplicaciones;

/* Crear base de datos y activarla */
CREATE DATABASE TiendasAplicaciones;
USE TiendasAplicaciones;

/* Para evitar errores, eliminar las tablas si estas existían antes */
DROP TABLE IF EXISTS empresa;
DROP TABLE IF EXISTS trabajan;
DROP TABLE IF EXISTS empleados;
DROP TABLE IF EXISTS telefono;
DROP TABLE IF EXISTS dirigen;
DROP TABLE IF EXISTS aplicaciones;
DROP TABLE IF EXISTS venden;
DROP TABLE IF EXISTS tienda;
DROP TABLE IF EXISTS descargan;
DROP TABLE IF EXISTS usuarios;

/* Código para crear cada una de las tablas junto con sus atributos, claves primarias, etc.
   empezando con las entidades*/
CREATE TABLE empresa (
   nombre_empresa  CHAR(40), /*Se permite texto hasta 40 caracteres*/ 
   correo VARCHAR(60) NOT NULL, /*Se permite texto hasta 60 caracteres (letras y numeros) y se fuerza a que no sean nulo*/ 
   pais CHAR(40) NOT NULL, /*Se permite texto hasta 40 caracteres y se fuerza a que no sea nulo*/ 
   año_creacion DATE  NOT NULL, /*Formato fecha no nulo*/ 
   telefono  NUMERIC(20,0) NOT NULL, /*Formato número hasta 20 dígitos para no limitar sólo a formato español y no nulo*/ 
   web VARCHAR(400) NOT NULL, /*Se permite texto hasta 400 caracteres que representará la URL de la web*/ 
   PRIMARY KEY(nombre_empresa) /*Se fija como clave primaria el nombre de la empresa*/ 
);
/*Se añade una restricción para forzar a que la fecha introducida sea del tipo año, mes, dia */
alter table empresa add constraint check (año_creacion LIKE 'yyyy-mm-dd');

/*Segunda Tabla, empleados */
CREATE TABLE empleados (
   dni_empleado VARCHAR(20), /*Mismo razonamiento a la hora de colocar las variables que la tabla anterior*/
   calle VARCHAR(40),
   numero INTEGER,
   cod_postal INTEGER,
   experiencia INTEGER,
   correo VARCHAR(60),
   PRIMARY KEY(dni_empleado)
);
/*Se añade restricción para evitar que la experiencia sea negativa (en años) */
alter table empleados add constraint check (experiencia > 0);

/*Se crea la tabla para el campo multivalorado teléfono*/
CREATE TABLE telefono (
   numtel NUMERIC(20,0), 
   dni_empleado VARCHAR(20), /*EL DNI del empleado se coloca para que sea del mismo tipo que en la tabla empleados*/
   PRIMARY KEY(numtel), /*La clave primaria será el número de teléfono*/
   FOREIGN KEY(dni_empleado) REFERENCES empleados(dni_empleado) ON DELETE cascade /*Se coloca como Foreign Key el DNI del empleado y con
   borrado en cascada para que si se elimina el empleado se borre su número */
);

/*Se crea la tabla para aplicaciones*/
CREATE TABLE aplicaciones (
   codigo_app   VARCHAR(30),
   espacio     BIGINT not null check (espacio>0), /*El espacio puede ser de muchos KB y se fuerza a que sea positivo*/
   precio  INTEGER,
   categoria   CHAR(20),
   nombre     VARCHAR(40),
   PRIMARY KEY(codigo_app)
   );
   
/*Se crea la tabla para tienda*/
CREATE TABLE tienda (
   nombre_tienda   VARCHAR(40), 
   propietaria   VARCHAR(30) NOT NULL,
   web VARCHAR(400) NOT NULL,
   PRIMARY KEY(nombre_tienda)
   );
   
/*Se crea la tabla para usuarios*/
CREATE TABLE usuarios (
   num_cuenta   VARCHAR(40), /*Número de cuenta y usuario se establecen como varchar por si contienen caracteres numéricos */
   nombre_usuario     varchar(40),
   calle VARCHAR(40),
   numero INTEGER,
   cod_postal INTEGER,
   telefono  NUMERIC(20,0) NOT NULL,
   PRIMARY KEY(num_cuenta)
   );
   
   /***************************************************************************************************/
   
   /*Comenzamos a crear las tablas de las relaciones */
   /* Tabla trabajan */
   CREATE TABLE trabajan (
   nombre_empresa  CHAR(40), 
   dni_empleado VARCHAR(20), /*EL DNI del empleado se coloca para que sea del mismo tipo que en la tabla empleados*/
   fecha_inicio DATE NOT NULL,
   fecha_fin DATE NOT NULL,
   PRIMARY KEY(nombre_empresa,dni_empleado), /*La clave primaria será la combinación de las claves primarias de empresa y empleado*/
   FOREIGN KEY(nombre_empresa) REFERENCES empleados(nombre_empresa) ON DELETE cascade,
   FOREIGN KEY(dni_empleado) REFERENCES empleados(dni_empleado) ON DELETE cascade /*Se coloca como Foreign Key el DNI del empleado y nombre de la empresa con
   borrado en cascada para que si se elimina o bien de la empresa o bien de empleados, desaparezca de trabajan */
);

   /* Tabla dirigen */
   CREATE TABLE dirigen (
   codigo_app  VARCHAR(30), 
   espacio     BIGINT NOT NULL CHECK (espacio>0), /*El espacio puede ser de muchos KB y se fuerza a que sea positivo*/
   precio  INTEGER,
   categoria   CHAR(20),
   nombre     VARCHAR(40),
   dni_empleado VARCHAR(20),
   fecha_inicio DATE NOT NULL,
   fecha_fin DATE NOT NULL,
   PRIMARY KEY(codigo_app), /*La clave primaria será la combinación de las claves primarias de empresa y empleado*/
   FOREIGN KEY(codigo_app) REFERENCES empleados(codigo_app) ON DELETE cascade,
   FOREIGN KEY(dni_empleado) REFERENCES empleados(dni_empleado) ON DELETE SET NULL /*Se coloca como Foreign Key el DNI del empleado y codigo de la app con
   borrado en cascada para que si se elimina de las aplicaciones, esta desaparezca y si se elimina el empleado, este quede como NULL */
   );
   
   /* Tabla venden */
   CREATE TABLE venden (
   codigo_app  VARCHAR(30), 
   nombre_tienda VARCHAR(40),
   PRIMARY KEY(codigo_app,nombre_tienda), /*La clave primaria será la combinación de las claves primarias de aplicaciones y tienda*/
   FOREIGN KEY(codigo_app) REFERENCES empleados(codigo_app) ON DELETE CASCADE,
   FOREIGN KEY(nombre_tienda) REFERENCES tienda(nombre_tienda) ON DELETE CASCADE /*Se coloca como Foreign Key el DNI del empleado y codigo de la app con
   borrado en cascada para que si se elimina de la tienda o de aplicaciones, desaparezca en venden*/
   );
   
   
   /* Tabla descargan */
   CREATE TABLE descargan (
   codigo_app  VARCHAR(30), 
   num_cuenta VARCHAR(40),
   comentario VARCHAR(40),
   puntuacion VARCHAR(40),
   PRIMARY KEY(codigo_app,num_cuenta), /*La clave primaria será la combinación de las claves primarias de aplicaciones y usuarios*/
   FOREIGN KEY(codigo_app) REFERENCES empleados(codigo_app) ON DELETE CASCADE,
   FOREIGN KEY(num_cuenta) REFERENCES usuarios(num_cuenta) ON DELETE CASCADE /*Se coloca como Foreign Key el codigo de aplicacion y cuenta de usuario con
   borrado en cascada para que si se elimina de la tienda o de usuarios, desaparezca en descargan*/
   );

INSERT INTO cliente VALUES ('008', 'Torcuato Montero', 'Rio  Duero 14', 937846308);
INSERT INTO cliente VALUES ('009', 'Asuncion Rodríguez', 'Pez 14', 914565308);
INSERT INTO cliente VALUES ('010', 'Eustquia Alonso', 'Rio Lozoya 35', 917845208);
INSERT INTO cliente VALUES ('011', 'Angela Callejo',  'Pedro Villar 330',  914849303);
INSERT INTO cliente VALUES ('012', 'Maribel Riocal',  'Luna 11', 914394943);
INSERT INTO cliente VALUES ('013', 'Juan Antonio Sanz', 'Clavel 21',      915656501);
INSERT INTO cliente VALUES ('014', 'Clara Garcia', 'Cercona 57', 913389307);
INSERT INTO cliente VALUES ('015', 'Isabel Sanrio', 'Travesia del rio 14', 917845308);
INSERT INTO cliente VALUES ('016', 'Eugenio Arribas', 'Tinajas 14', 917845308);

INSERT INTO articulo(codArticulo, denom,precio,unidades) VALUES ('0001', 'Ordenador Sobremesa',     600, 12);
INSERT INTO articulo(codArticulo, denom,precio,unidades) VALUES ('0002', 'Ordenador Portátil',     1000,  6);
INSERT INTO articulo(codArticulo, denom,precio,unidades) VALUES ('0003', 'Tarjeta Red',         20, 25);
INSERT INTO articulo(codArticulo, denom,precio,unidades) VALUES ('0004', 'Impresora Láser',    200,  4);
INSERT INTO articulo(codArticulo, denom,precio,unidades) VALUES ('0005', 'Ratón USB',            7, 50);
INSERT INTO articulo(codArticulo, denom,precio,unidades) VALUES ('0006', 'Monitor TFT',        250, 10);
INSERT INTO articulo(codArticulo, denom,precio,unidades) VALUES ('0007', 'Router inalámbrico', 100, 30);
INSERT INTO articulo(codArticulo, denom,precio,unidades) VALUES ('0008', 'altavoz', 100,30);


INSERT INTO compra VALUES('011', '0001', '2016/10/06', 1);
INSERT INTO compra VALUES('011', '0005', '2017/10/06', 2);
INSERT INTO compra VALUES('012', '0002', '2019/11/06', 1);
INSERT INTO compra VALUES('012', '0003', '2018/11/06', 3);
INSERT INTO compra VALUES('012', '0001', '2018/11/06', 3);
INSERT INTO compra VALUES('012', '0004', '2018/11/06', 3);
INSERT INTO compra VALUES('012', '0005', '2018/11/06', 3);
INSERT INTO compra VALUES('012', '0006', '2018/11/06', 3);
INSERT INTO compra VALUES('012', '0007', '2018/11/06', 3);
INSERT INTO compra VALUES('012', '0008', '2018/11/06', 3);
INSERT INTO compra VALUES('013', '0006', '2020/10/06', 2);
INSERT INTO compra VALUES('013', '0003', '2017/10/06', 2);
INSERT INTO compra VALUES('015', '0004', '2003/11/06', 1);
INSERT INTO compra VALUES('015', '0002', '2019/11/06', 1);
INSERT INTO compra VALUES('015', '0007', '2020/11/06', 8);

-- Tabla de clientes con todos los campos

select *
from cliente;

-- Tabla de articulos con todos los campos
select *
from articulo;

-- tabla compra con todos sus campos
select *
from compra;
-- directorio dónde guardar los archivos de datos
-- SELECT @@GLOBAL.secure_file_priv;

-- Cargar datos de un archivo externo
/*
LOAD DATA  INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/persona.csv'
 INTO TABLE cliente
 FIELDS TERMINATED BY ';'
 LINES TERMINATED BY '\n'
 IGNORE 1 ROWS;
*/
select * from cliente ;
select 'hola';

select TIMESTAMPDIFF(year,'1900-3-28',CURDATE())  ;-- calcular la edad de una persona

-- now() momento actual
select now();

-- now()-interval 5 year momento actual menos 5 años

select now()-interval 5 year;

-- month(curdate()) mes de la fecha actual
select month(curdate());

-- case when
select *, (case when (precio>100)   THEN  round(precio * 1.05,1)				 
  ELSE round(precio * 1.00,2) end  ) as precioIncrementado 
  from articulo;
  
 -- Escribir  todos los campos de la tabla articulo
 select  * from articulo; 
  
-- Listado de clientes en orden inverso al alfabético.
select nombreC
from cliente
order by nombreC desc;

-- Código de los clientes que han realizado compras,
--  no se debe repetir el código del cliente si éste ha comprado 2 veces.
select distinct idCliente
from compra;
-- Si no escribimos distinct, se escriben todos los clientes, si han comprado varias veces se escriben repetidos
select  idCliente
from compra;

-- articulo que valen mas de 10 euros
select denom,precio
from articulo
where precio>10;

-- articulo que valen mas de 10 euros y menor que 100
select denom,precio
from articulo
where precio>10 and precio<100;

select denom,precio
from articulo
where precio between 10 and 99;

-- Nombre y precio de cada uno de los artículos si les incrementamos el precio el 10%. 
-- No modifica la tabla, para que una tabla se modifique deberíamos usar un update
select denom 'Nombre artículo', precio*1.1 'incremento de un 10'
from articulo;
-- Incrementar el precio de los artículos con menos de 5 unidades un 10%
/*
select * from articulo;
update articulo 
set precio=precio*1.1, descuento=50,precioFinal=PRECIO-PRECIO*(DESCUENTO/100)
where unidades<5;
select * from articulo;
*/ 

-- Nombre de los artículos de los que me quedan entre 3 y 10 unidades.
select denom, unidades
from articulo
where unidades between 3 and 10;

select denom, unidades
from articulo
where unidades>= 3 and  unidades <=10;

-- Nombre de los artículos ordenados descendentemente por el número de 
-- unidades que nos quedan en el almacén.
select denom, unidades
from articulo
order by unidades ;

-- Nombre del artículo que tiene más unidades 
select denom, unidades
from articulo
order by unidades desc
limit 1;-- con limit especificamos el número de tuplas que queremos escribir

-- Código de los artículos adquiridos por el cliente 015
select idArticulo, denom  'nombre articulo'
from compra   c, articulo  a
where idCliente='015' and c.idArticulo=a.codArticulo; 

-- Nombre del cliente que ha comprado una unidad entre el 2000 y el 2005
select distinct nombreC
from compra as c,cliente as cli
where c.idCliente=cli.codCliente and fecCompra  between '2000/01/01' and '2005/12/31';

-- nombre de los ARTICULOS que tienen mas unidades en almacen que el 
-- articulo '0001'
select a2.denom, a2.unidades,a2.CodArticulo
from articulo as a, articulo as a2
where a.codArticulo='0001' and a.unidades <= a2.unidades;

-- gastos de los clientes que han comprado alguna vez
select idCliente, nombreC, sum(unidades*precio) as total
from compra as c,cliente as cli ,articulo as a
where c.IdArticulo=a.codArticulo and c.IdCliente=cli.codCliente
group by idCliente;

-- gastos superiores a 7000 de los clientes que ha comprado alguna vez
select idCliente, nombreC, sum(unidades*precio) as total
from compra as c,cliente as cli ,articulo as a
where c.IdArticulo=a.codArticulo and c.IdCliente=cli.codCliente
group by idCliente
having total >7000;-- cuando son resultados de las funciones de agregación se comparan en having

-- Articulos que aún no se han comprado
select denom
from articulo
where codArticulo not in (select distinct(idArticulo)
                          from compra);

select denom
from articulo as a left join compra as c on 
a.codArticulo =c.idArticulo
where idArticulo is NULL; 
   
select denom
from compra as c right join articulo as a on 
a.codArticulo =c.idArticulo
where idArticulo is NULL;                 

-- clientes que han comprado al menos una unidad de todos los articulos
-- existentes en almacen
select idCliente,nombreC, count(distinct idArticulo) as totalArticulos
from compra as c inner join cliente as cli on  c.IdCliente=cli.codCliente
group by idCliente
having totalArticulos = (select count(codArticulo)
                          from articulo);
                          
   -- articulos vendidos cuyo precio es menor que el precio medio de todos los ariculos
   select distinct denom,precio
   from articulo as a inner join compra as c on c.idArticulo=a.codArticulo
   where   precio < (select avg(precio)
                      from articulo);
   
	select distinct denom,precio
   from articulo as a,compra as c
   where c.idArticulo=a.codArticulo and precio < (select avg(precio)
                                                    from articulo);

-- 	Código de los clientes que han comprado algún artículo este año
select codCliente, nombreC
from compra as c, cliente as cli
where c.IdCliente=cli.codCliente and year(fecCompra)=year(curdate());

select codCliente, nombreC
from compra as c inner join cliente as cli on c.IdCliente=cli.codCliente
where  year(fecCompra)=year(curdate());

-- Nombre  de los clientes y los artículos comprados
select nombreC, denom
from compra as c inner join cliente as cli on  c.IdCliente=cli.codCliente inner join articulo as a on
 c.IdArticulo=a.codArticulo 
order by idCliente;

-- 	Listado del cliente y el número total de unidades de artículos adquiridos por éste 
select idCliente,nombreC, sum(numUnidades) totalUnidades
from compra as c,cliente as cli ,articulo as a
where c.IdArticulo=a.codArticulo and c.IdCliente=cli.codCliente
group by idCliente;


-- 	Precio medio de los artículos del almacén
select avg(precio)
from articulo;

-- Listado con el número de unidades compradas de cada artículo
select idArticulo,denom, sum(numUnidades) totalUnidades
from compra as c,articulo as a
where c.IdArticulo=a.codArticulo 
group by idArticulo;

select idArticulo,denom, sum(numUnidades) totalUnidades
from compra as c inner join articulo as a on  c.IdArticulo=a.codArticulo 
group by idArticulo;

-- Nombre del cliente que más unidades ha adquirido de algún artículo 
select distinct(idCliente),nombreC
from compra as c,cliente as cli 
where c.IdCliente=cli.codCliente and numUnidades =(select max(numUnidades)
                                                   from compra);
select idCliente,nombreC, numUnidades
from compra as c inner join cliente as cli  on c.IdCliente=cli.codCliente
order by numUnidades desc
limit 1;

-- Clientes que han realizado más de 2 compras
-- agrupamos por el nombre de cliente y contamos las filas que tiene ese cliente, 
-- que vienen determinada por el artículo
select nombreC, count(idArticulo) numeroCompras
from cliente, compra
where codCliente=idCliente
group by nombreC
having numeroCompras >2;

select nombreC, count(idArticulo) numeroCompras
from cliente as cli inner join  compra as c on cli.codCliente=c.idCliente
group by nombreC
having numeroCompras >2;

-- vista gastos por cliente
create view gastoPorCliente as
select nombreC, sum(precio*numUnidades) total
from articulo as a, cliente as cli, compra as p
where codCliente=IdCliente and codArticulo=idArticulo
group by codCliente
order by nombreC desc;

select * from gastoPorCliente;
select nombreC, total
from gastoPorCliente
where total=(select max(total)
			 from gastoPorCliente);