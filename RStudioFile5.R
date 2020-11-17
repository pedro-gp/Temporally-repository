ggplot(data = millas) + 
  geom_point(mapping = aes(x = cilindrada, y = autopista)) + 
  facet_grid(traccion~cilindros)

ggplot(data = millas) + 
  geom_point(mapping = aes(x = traccion, y = cilindros))  

ggplot(data = millas) + 
  geom_point(mapping = aes(x = cilindrada, y = autopista)) + 
  facet_grid(.~cilindros)

ggplot(data = millas) + 
  geom_point(mapping = aes(x = cilindrada, y = autopista)) + 
  facet_grid(traccion~.)


ggplot(data = millas) + 
  geom_smooth(
    mapping = aes(x = cilindrada, y = autopista, color = traccion),method = 'loess',
    formula = y ~ x,
    show.legend = FALSE
  
ggplot(data = millas) + 
  geom_point(mapping = aes(x = cilindrada, y = autopista), colour = 'blue') +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista), method = 'loess',
              formula = y ~ x, colour = 'red')


ggplot(data = millas, mapping = aes(x = cilindrada, y = autopista)) +
  geom_point(shape = 2) + 
  geom_smooth(colour = 'blue')

ggplot(data = millas, mapping = aes(x = cilindrada, y = autopista)) +
  geom_point(mapping = aes(colour = clase)) + 
  geom_smooth(method = 'loess', formula = y ~ x)

ggplot(data = millas, mapping = aes(x = cilindrada, y = autopista)) +
  geom_point(mapping = aes(colour = clase)) + 
  geom_smooth(data = filter(millas, clase == 'subcompacto'), se = FALSE)

ggplot(data = millas, mapping = aes(x = cilindrada, y = autopista)) +
  geom_point(mapping = aes(colour = clase)) + 
  geom_smooth(data = filter(millas, clase == 'suv'), se = TRUE)

ggplot(data = millas, mapping = aes(x = cilindrada, y = autopista, colour = traccion)) +
  geom_point() + 
  geom_smooth(se = FALSE)


# Transformaciones estadísticas
dataset = diamantes
View(dataset)

ggplot(data = dataset) + 
  geom_bar(mapping = aes(x = corte))

ggplot(data = dataset) + 
  stat_count(mapping = aes(x = corte))

demo <- tribble(
  ~corte, ~freq,
  'Regular',1610,
  'Bueno', 4906,
  'Muy Bueno', 12802,
  'Premium',13791,
  'Ideal',215551
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = corte, y = freq), stat = 'identity')

ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte,
                         y = stat(prop), group = 1))
