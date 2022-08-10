FROM openjdk:8-alpine

COPY target/uberjar/clojseq.jar /clojseq/app.jar

EXPOSE 3000

CMD ["java", "-jar", "/clojseq/app.jar"]
