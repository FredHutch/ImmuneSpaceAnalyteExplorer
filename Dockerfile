FROM fredhutch/r-shiny-server-base:latest
RUN apt-get update -y
RUN apt-get install -y pandoc nginx supervisor
# RUN useradd -u 5555 -m -d /home/shiny -c "shiny user" shiny

RUN rm -rf /srv/shiny-server/
ADD app/. /srv/shiny-server/

# ADD app/. /home/shiny/
ADD system/. /home/shiny/system/
RUN chown -R shiny:shiny /home/shiny 
WORKDIR /home/shiny
# USER shiny
EXPOSE 3838
# CMD Rscript start.R 
CMD ["/bin/sh", "-c", "/usr/bin/supervisord -c /home/shiny/system/sup.conf"]