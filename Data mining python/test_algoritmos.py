from algoritmos import apriori, eclat, spade, generar_reglas_asociacion

# Dataset de prueba para transacciones (Apriori y ECLAT)
transactions = [
    ['leche', 'pan', 'pañal'],
    ['leche', 'pañal', 'cerveza', 'huevos'],
    ['pan', 'pañal', 'cerveza'],
    ['leche', 'pan', 'pañal', 'cerveza'],
    ['leche', 'pan', 'cerveza']
]

print("=== APRIORI ===")
frecuentes_apriori = apriori(transactions, min_support=0.5)
for itemset, sup in frecuentes_apriori.items():
    print(f"{set(itemset)}: soporte = {sup:.2f}")

print("\n=== REGLAS DE ASOCIACIÓN (Apriori) ===")
reglas = generar_reglas_asociacion(frecuentes_apriori, min_confidence=0.6)
for r in reglas:
    print(f"{set(r['antecedente'])} => {set(r['consecuente'])} | sop: {r['soporte']:.2f}, conf: {r['confianza']:.2f}, lift: {r['lift']:.2f}")

print("\n=== ECLAT ===")
frecuentes_eclat = eclat(transactions, min_support=0.5)
for itemset, sup in frecuentes_eclat.items():
    print(f"{set(itemset)}: soporte = {sup:.2f}")

# Dataset de prueba para secuencias (SPADE)
secuencias = [
    [{'A'}, {'B', 'C'}, {'D'}],
    [{'A', 'B'}, {'C'}],
    [{'A'}, {'B'}, {'C'}],
    [{'B'}, {'C'}]
]

print("\n=== SPADE ===")
patrones_spade = spade(secuencias, min_support=0.5)
for pattern, sup in patrones_spade.items():
    print(f"{pattern}: soporte = {sup:.2f}")
