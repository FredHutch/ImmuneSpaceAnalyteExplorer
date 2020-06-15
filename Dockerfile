FROM fredhutch/r-shiny-base:latest
RUN apt-get update
RUN apt-get install -y pandoc nginx supervisor
RUN useradd -u 5555 -m -d /home/shiny -c "shiny user" shiny
ADD app/. /home/shiny/
ADD system/. /home/shiny/system/
RUN chown -R shiny:shiny /home/shiny 
WORKDIR /home/shiny
# USER shiny
EXPOSE 8888
# CMD Rscript start.R 
CMD ["/bin/sh", "-c", "/usr/bin/supervisord -c /home/shiny/system/sup.conf"]