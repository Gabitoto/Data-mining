"""
Módulo de Algoritmos de Minería de Datos
-----------------------------------------
Este módulo contiene la implementación de los algoritmos clásicos de minería de patrones:
- Apriori (Conjuntos de ítems frecuentes y Reglas de Asociación)
- ECLAT (Equivalence Class Transformation - Minería de ítems frecuentes en formato vertical)
- SPADE (Sequential Pattern Discovery using Equivalence classes - Minería de patrones secuenciales)

Uso:
----
from algoritmos import apriori, eclat, spade, generar_reglas_asociacion
"""

from collections import defaultdict
from itertools import combinations
from typing import Any, Dict, List, Set, Tuple, Union


# ==============================================================================
# 1. ALGORITMO APRIORI Y REGLAS DE ASOCIACIÓN
# ==============================================================================

def apriori(dataset: List[Union[List, Set, Tuple]], min_support: float = 0.5) -> Dict[frozenset, float]:
    """
    Algoritmo Apriori para la extracción de conjuntos de ítems frecuentes.

    Parámetros
    ----------
    dataset : List[Union[List, Set, Tuple]]
        Lista de transacciones, donde cada transacción es una colección de ítems.
        Ejemplo: [['leche', 'pan'], ['leche', 'pañal', 'cerveza'], ...]
    min_support : float
        Soporte mínimo (valor entre 0.0 y 1.0, o número entero de transacciones si > 1).

    Retorna
    -------
    Dict[frozenset, float]
        Diccionario con los conjuntos frecuentes (frozenset) y su nivel de soporte (0.0 a 1.0).

    Ejemplo
    -------
    >>> data = [['A', 'B'], ['A', 'C'], ['A', 'B', 'C']]
    >>> result = apriori(data, min_support=0.5)
    """
    if not dataset:
        return {}

    n_transactions = len(dataset)
    min_count = min_support * n_transactions if min_support <= 1.0 else min_support

    # Normalizar transacciones a conjuntos
    transactions = [set(t) for t in dataset]

    # Paso 1: Ítems individuales (C1 -> L1)
    item_counts = defaultdict(int)
    for transaction in transactions:
        for item in transaction:
            item_counts[frozenset([item])] += 1

    current_frequent = {itemset: count for itemset, count in item_counts.items() if count >= min_count}
    all_frequent = dict(current_frequent)

    k = 2
    while current_frequent:
        # Generar candidatos C_k a partir de L_{k-1}
        prev_itemsets = list(current_frequent.keys())
        candidate_itemsets = set()

        for i in range(len(prev_itemsets)):
            for j in range(i + 1, len(prev_itemsets)):
                union_set = prev_itemsets[i] | prev_itemsets[j]
                if len(union_set) == k:
                    # Poda apriori: todos los subconjuntos de tamaño k-1 deben ser frecuentes
                    subsets_k_minus_1 = [frozenset(s) for s in combinations(union_set, k - 1)]
                    if all(s in current_frequent for s in subsets_k_minus_1):
                        candidate_itemsets.add(union_set)

        # Contar soporte de los candidatos C_k en la base de datos
        candidate_counts = defaultdict(int)
        for transaction in transactions:
            for candidate in candidate_itemsets:
                if candidate.issubset(transaction):
                    candidate_counts[candidate] += 1

        # Filtrar L_k por min_support
        current_frequent = {itemset: count for itemset, count in candidate_counts.items() if count >= min_count}
        all_frequent.update(current_frequent)
        k += 1

    # Convertir conteos absolutos a frecuencias relativas (soporte)
    return {itemset: count / n_transactions for itemset, count in all_frequent.items()}


def generar_reglas_asociacion(frecuentes: Dict[frozenset, float], min_confidence: float = 0.7) -> List[Dict[str, Any]]:
    """
    Genera reglas de asociación a partir de los conjuntos de ítems frecuentes.

    Parámetros
    ----------
    frecuentes : Dict[frozenset, float]
        Diccionario devuelto por apriori o eclat {frozenset: soporte}.
    min_confidence : float
        Confianza mínima entre 0.0 y 1.0.

    Retorna
    -------
    List[Dict[str, Any]]
        Lista de diccionarios con las reglas: antecedente, consecuente, soporte, confianza, lift.
    """
    reglas = []

    for itemset, soporte_itemset in frecuentes.items():
        if len(itemset) < 2:
            continue

        # Generar todos los antecedentes posibles
        items = list(itemset)
        for r in range(1, len(items)):
            for antecedente_tuple in combinations(items, r):
                antecedente = frozenset(antecedente_tuple)
                consecuente = itemset - antecedente

                if antecedente in frecuentes:
                    soporte_antecedente = frecuentes[antecedente]
                    confianza = soporte_itemset / soporte_antecedente

                    if confianza >= min_confidence:
                        soporte_consecuente = frecuentes.get(consecuente, 0.0)
                        lift = confianza / soporte_consecuente if soporte_consecuente > 0 else 0.0

                        reglas.append({
                            'antecedente': antecedente,
                            'consecuente': consecuente,
                            'soporte': soporte_itemset,
                            'confianza': confianza,
                            'lift': lift
                        })

    return reglas


# ==============================================================================
# 2. ALGORITMO ECLAT (Equivalence Class Transformation)
# ==============================================================================

def eclat(dataset: List[Union[List, Set, Tuple]], min_support: float = 0.5) -> Dict[frozenset, float]:
    """
    Algoritmo ECLAT para la extracción de conjuntos de ítems frecuentes en layout vertical.

    Parámetros
    ----------
    dataset : List[Union[List, Set, Tuple]]
        Lista de transacciones en formato horizontal.
    min_support : float
        Soporte mínimo (expresado entre 0.0 y 1.0, o entero de transacciones).

    Retorna
    -------
    Dict[frozenset, float]
        Diccionario {frozenset: soporte}.
    """
    if not dataset:
        return {}

    n_transactions = len(dataset)
    min_count = min_support * n_transactions if min_support <= 1.0 else min_support

    # Construir base de datos vertical: ítem -> conjunto de IDs de transacción (TID-set)
    vertical_db = defaultdict(set)
    for tid, transaction in enumerate(dataset):
        for item in transaction:
            vertical_db[frozenset([item])].add(tid)

    # Filtrar ítems de tamaño 1 frecuentes
    frequent_1 = [(itemset, tids) for itemset, tids in vertical_db.items() if len(tids) >= min_count]

    # Ordenar por soporte para optimizar el árbol de búsqueda
    frequent_1.sort(key=lambda x: len(x[1]))

    resultados = {}

    def _eclat_recursive(prefix_itemset: frozenset, prefix_tidset: set, candidates: List[Tuple[frozenset, set]]):
        for i in range(len(candidates)):
            itemset_i, tidset_i = candidates[i]

            new_itemset = prefix_itemset | itemset_i
            new_tidset = prefix_tidset & tidset_i if prefix_tidset else set(tidset_i)

            if len(new_tidset) >= min_count:
                resultados[new_itemset] = len(new_tidset) / n_transactions

                # Generar candidatos subsecuentes
                new_candidates = []
                for j in range(i + 1, len(candidates)):
                    itemset_j, tidset_j = candidates[j]
                    new_candidates.append((itemset_j, tidset_j))

                if new_candidates:
                    _eclat_recursive(new_itemset, new_tidset, new_candidates)

    for i in range(len(frequent_1)):
        itemset, tids = frequent_1[i]
        resultados[itemset] = len(tids) / n_transactions

        candidates = frequent_1[i + 1:]
        if candidates:
            _eclat_recursive(itemset, tids, candidates)

    return resultados


# ==============================================================================
# 3. ALGORITMO SPADE (Sequential Pattern Discovery using Equivalence Classes)
# ==============================================================================

def spade(secuencias: List[List[Union[Set, List, Tuple]]], min_support: float = 0.5) -> Dict[Tuple[Tuple[str, ...], ...], float]:
    """
    Algoritmo SPADE para minería de patrones secuenciales frecuentes.

    Parámetros
    ----------
    secuencias : List[List[Union[Set, List, Tuple]]]
        Lista de secuencias de la base de datos.
        Cada secuencia es una lista de eventos u ordenamientos temporales de ítems.
        Ejemplo:
            [
                [{'A'}, {'B', 'C'}, {'D'}],
                [{'A', 'B'}, {'C'}],
                [{'B'}, {'C'}]
            ]
        O de manera simplificada si cada evento tiene un solo ítem:
            [['A', 'B', 'C'], ['A', 'C']] -> se interpreta como <(A), (B), (C)>
    min_support : float
        Soporte mínimo (expresado entre 0.0 y 1.0, o conteo absoluto).

    Retorna
    -------
    Dict[Tuple[Tuple[str, ...], ...], float]
        Diccionario donde las claves son tuplas que representan la secuencia frecuente, y los valores su soporte.
        Ejemplo de patrón: (('A',), ('B', 'C')) -> representa la secuencia <(A), (B, C)>
    """
    if not secuencias:
        return {}

    n_sequences = len(secuencias)
    min_count = min_support * n_sequences if min_support <= 1.0 else min_support

    # Normalizar secuencias a listas de tuplas ordenadas (eventos)
    normalized_seqs = []
    for seq in secuencias:
        norm_events = []
        for event in seq:
            if isinstance(event, (set, list, tuple)):
                norm_events.append(tuple(sorted(list(event))))
            else:
                norm_events.append((event,))
        normalized_seqs.append(norm_events)

    # Paso 1: Generar formato ID-List vertical para 1-secuencias <(item,)>
    # Estructura de ID-list: Dict[Pattern, Set[(SID, EID)]]
    # donde SID es ID de la secuencia (0..N-1) y EID es el ID del evento (tiempo 0..M-1)
    id_lists = defaultdict(set)

    for sid, seq in enumerate(normalized_seqs):
        for eid, event in enumerate(seq):
            for item in event:
                pattern = ((item,),)
                id_lists[pattern].add((sid, eid))

    # Filtrar 1-secuencias frecuentes por número distinto de SIDs
    def _support(id_list: Set[Tuple[int, int]]) -> float:
        sids = {sid for sid, eid in id_list}
        return len(sids)

    frequent_patterns = {}
    current_id_lists = {}

    for pattern, id_list in id_lists.items():
        sup_count = _support(id_list)
        if sup_count >= min_count:
            frequent_patterns[pattern] = sup_count / n_sequences
            current_id_lists[pattern] = id_list

    # Función auxiliar para unir dos ID-lists y generar nuevos patrones secuenciales
    def _join_id_lists(p1: Tuple[Tuple[str, ...], ...], id_list1: Set[Tuple[int, int]],
                       p2: Tuple[Tuple[str, ...], ...], id_list2: Set[Tuple[int, int]]) -> Dict[Tuple[Tuple[str, ...], ...], Set[Tuple[int, int]]]:
        joined = defaultdict(set)

        # Caso A: Unir último ítem en el mismo evento (si p1 y p2 pueden fusionarse en el mismo evento)
        # Ejemplo: <(A)> y <(B)> -> <(A, B)>
        if len(p1) == len(p2) and p1[:-1] == p2[:-1]:
            last1, last2 = p1[-1], p2[-1]
            if len(last1) == 1 and len(last2) == 1 and last1[0] < last2[0]:
                new_event = tuple(sorted(list(last1 + last2)))
                new_pattern = p1[:-1] + (new_event,)
                # Intersección en el mismo EID
                for sid1, eid1 in id_list1:
                    if (sid1, eid1) in id_list2:
                        joined[new_pattern].add((sid1, eid1))

        # Caso B: Unir como evento subsecuente (secuencia temporal)
        # <P1> seguido de <ultimo de P2>
        last2_itemset = p2[-1]
        new_pattern_seq = p1 + (last2_itemset,)

        # SID iguales y EID1 < EID2
        sids1_map = defaultdict(list)
        for sid, eid in id_list1:
            sids1_map[sid].append(eid)

        for sid2, eid2 in id_list2:
            if sid2 in sids1_map:
                for eid1 in sids1_map[sid2]:
                    if eid1 < eid2:
                        joined[new_pattern_seq].add((sid2, eid2))

        return joined

    # DFS Recursivo para explorar secuencias más largas
    def _spade_dfs(id_lists_dict: Dict[Tuple[Tuple[str, ...], ...], Set[Tuple[int, int]]]):
        patterns = list(id_lists_dict.keys())
        for i in range(len(patterns)):
            p1 = patterns[i]
            id_l1 = id_lists_dict[p1]

            next_id_lists = {}
            for j in range(len(patterns)):
                p2 = patterns[j]
                id_l2 = id_lists_dict[p2]

                candidates = _join_id_lists(p1, id_l1, p2, id_l2)
                for new_p, new_id_list in candidates.items():
                    if new_p not in frequent_patterns:
                        sup_count = _support(new_id_list)
                        if sup_count >= min_count:
                            frequent_patterns[new_p] = sup_count / n_sequences
                            next_id_lists[new_p] = new_id_list

            if next_id_lists:
                _spade_dfs(next_id_lists)

    _spade_dfs(current_id_lists)
    return frequent_patterns
